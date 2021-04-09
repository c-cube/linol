
(** {1 Simple JSON-RPC2 implementation}
    See {{: https://www.jsonrpc.org/specification} the spec} *)

module Fmt = CCFormat
module J = Yojson.Safe
module Err = Jsonrpc.Response.Error

type json = Yojson.Safe.t

module type IO = Sigs.IO

module type S = sig
  module IO : IO

  type t
  (** A jsonrpc2 connection. *)

  include module type of Server.Make(IO)

  val create :
    ic:IO.in_channel ->
    oc:IO.out_channel ->
    server ->
    t
  (** Create a connection from the pair of channels *)

  val create_stdio : server -> t
  (** Create a connection using stdin/stdout *)

  val run :
    ?shutdown:(unit -> bool) ->
    t -> unit IO.t
  (** Listen for incoming messages and responses *)
end

module Make(IO : IO)
    : S with module IO = IO
= struct
  module IO = IO
  include Server.Make(IO)
  open IO

  type json = J.t

  let spf = Printf.sprintf

  module ErrorCode = Lsp.Types.ErrorCodes
  (*
  module Err = struct
    type code = int
    let code_parse_error : code = (-32700)
    let code_invalid_request : code = (-32600)
    let code_method_not_found : code = (-32601)
    let code_invalid_param : code = (-32602)
    let code_internal_error : code = (-32603)
  end
                 *)

  exception E of ErrorCode.t * string

  (* bind on IO+result *)
  let ( let*? ) x f =
    let* x = x in
    match x with
    | Ok x -> f x
    | Error _ as err -> IO.return err

  type t = {
    ic: IO.in_channel;
    oc: IO.out_channel;
    s: server;
  }

  let create ~ic ~oc server : t = {ic; oc; s=server}

  let create_stdio server : t =
    create ~ic:IO.stdin ~oc:IO.stdout server

  (* send a single message *)
  let send_json_ (self:t) (j:json) : unit IO.t =
    let json = J.to_string j in
    Log.debug (fun k->k "jsonrpc2: send json: %s" json);
    let full_s =
      Printf.sprintf "Content-Length: %d\r\n\r\n%s"
        (String.length json) json
    in
    IO.write_string self.oc full_s

  let send_response (self:t) (m:Jsonrpc.Response.t) : unit IO.t =
    let json = Jsonrpc.Response.yojson_of_t m in
    send_json_ self json

  let send_server_notif (self:t) (m:Jsonrpc.Message.notification) : unit IO.t =
    let json = Jsonrpc.Message.yojson_of_notification m in
    send_json_ self json

  let try_ f =
    IO.catch
      (fun () -> let+ x = f() in Ok x)
      (fun e -> IO.return (Error e))

  (* read a full message *)
  let read_msg (self:t) : (Jsonrpc.Message.either, exn) result IO.t =
    let rec read_headers acc =
      let*? line =
        try_ @@ fun () -> IO.read_line self.ic
      in
      match String.trim line with
      | "" -> IO.return (Ok acc) (* last separator *)
      | line ->
        begin match
            let i = String.index line ':' in
            if i<0 || String.get line (i+1) <> ' ' then raise Not_found;
            let key = String.lowercase_ascii @@ String.sub line 0 i in
            let v =
              String.lowercase_ascii @@
              String.trim (String.sub line (i+1) (String.length line-i-1))
            in
            key, v
          with
          | pair -> read_headers (pair :: acc)
          | exception _ ->
            IO.return (Error (E(ErrorCode.ParseError, spf "invalid header: %S" line)))
        end
    in
    let*? headers = read_headers [] in
    Log.debug (fun k->k "jsonrpc2: read headers: %a" Fmt.Dump.(list @@ pair string string) headers);
    let ok = match List.assoc "content-type" headers with
      | "utf8" | "utf-8" -> true
      | _ -> false
      | exception Not_found -> true
    in
    if ok then (
      match int_of_string (List.assoc "content-length" headers) with
      | n ->
        Log.debug (fun k->k "jsonrpc2: read %d bytes..." n);
        let buf = Bytes.make n '\000' in
        let*? () =
          try_ @@ fun () -> IO.read self.ic buf 0 n
        in
        (* log_lsp_ "got bytes %S" (Bytes.unsafe_to_string buf); *)
        let*? j =
          try_ @@ fun () ->
          IO.return @@ J.from_string (Bytes.unsafe_to_string buf)
        in
        Log.debug (fun k->k "got json %s" (J.to_string j));
        begin match Jsonrpc.Message.either_of_yojson j with
          | m -> IO.return @@ Ok m
          | exception _ ->
            Log.err (fun k->k "cannot decode json message");
            IO.return (Error (E(ErrorCode.ParseError, "cannot decode json")))
        end
      | exception _ ->
        IO.return @@
        Error (E(ErrorCode.ParseError, "missing content-length' header"))
    ) else (
      IO.return @@
      Error (E(ErrorCode.InvalidRequest, "content-type must be 'utf-8'"))
    )

  let run ?(shutdown=fun _ -> false) (self:t) : unit IO.t =
    let process_msg r =
      let module M = Jsonrpc.Message in
      let protect ~id f =
        IO.catch f
          (fun e ->
             let message = spf "%s\n%s" (Printexc.to_string e) (Printexc.get_backtrace()) in
             let r = Jsonrpc.Response.error id
               (Jsonrpc.Response.Error.make
                 ~code:Jsonrpc.Response.Error.Code.InternalError
                 ~message ())
            in
            send_response self r)
      in
      match r.M.id with
      | None ->
        (* notification *)
        begin match Lsp.Client_notification.of_jsonrpc {r with M.id=()} with
          | Ok n ->
            IO.catch
              (fun () ->
                (self.s)#on_notification n
                  ~notify_back:(fun n ->
                      let msg = Lsp.Server_notification.to_jsonrpc n in
                      send_server_notif self msg))
              (fun e ->
                 let msg =
                   Lsp.Types.ShowMessageParams.create ~type_:Lsp.Types.MessageType.Error
                     ~message:(Printexc.to_string e)
                 in
                 let msg =
                   Lsp.Server_notification.LogMessage msg
                   |> Lsp.Server_notification.to_jsonrpc
                 in
                 send_server_notif self msg)
          | Error e ->
            IO.failwith (spf "cannot decode notification: %s" e)
        end
      | Some id ->
        (* request, so we need to reply *)
        IO.catch
          (fun () ->
            begin match Lsp.Client_request.of_jsonrpc {r with M.id} with
              | Ok (Lsp.Client_request.E r) ->
                protect ~id (fun () ->
                  let* reply = self.s#on_request r
                    ~notify_back:(fun n ->
                        let msg = Lsp.Server_notification.to_jsonrpc n in
                        send_server_notif self msg)
                  in
                  let reply_json = Lsp.Client_request.yojson_of_result r reply in
                  let response = Jsonrpc.Response.ok id reply_json in
                  send_response self response
                )
              | Error e ->
                IO.failwith (spf "cannot decode request: %s" e)
            end)
          (fun e ->
            let message = spf "%s\n%s" (Printexc.to_string e) (Printexc.get_backtrace()) in
            let r =
              Jsonrpc.Response.error id
              (Jsonrpc.Response.Error.make
                ~code:Jsonrpc.Response.Error.Code.InternalError
                ~message ())
            in
            send_response self r)
    in
    let rec loop () =
      if shutdown() then IO.return ()
      else (
        let* r = read_msg self in
        match r with
        | Ok r ->
          IO.spawn (fun () -> process_msg r);
          loop()
        | Error e -> IO.fail e
      )
    in
    loop()
end

