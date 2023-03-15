(** {1 Simple JSON-RPC2 implementation}
    See {{: https://www.jsonrpc.org/specification} the spec} *)

module J = Yojson.Safe
module Err = Jsonrpc.Response.Error

type json = Yojson.Safe.t

module type IO = Sigs.IO

module type S = sig
  module IO : IO

  type t
  (** A jsonrpc2 connection. *)

  include module type of Server.Make (IO)

  val create : ic:IO.in_channel -> oc:IO.out_channel -> server -> t
  (** Create a connection from the pair of channels *)

  val create_stdio : server -> t
  (** Create a connection using stdin/stdout *)

  val run : ?shutdown:(unit -> bool) -> t -> unit IO.t
  (** Listen for incoming messages and responses *)
end

module Make (IO : IO) : S with module IO = IO = struct
  module IO = IO
  include Server.Make (IO)
  open IO

  type json = J.t

  let spf = Printf.sprintf

  module ErrorCode = Jsonrpc.Response.Error.Code
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
    mutable id_counter: int;
    pending_responses: (Req_id.t, server_request_handler_pair) Hashtbl.t;
  }

  let create ~ic ~oc server : t =
    { ic; oc; s = server; id_counter = 0; pending_responses = Hashtbl.create 8 }

  let create_stdio server : t = create ~ic:IO.stdin ~oc:IO.stdout server

  (* send a single message *)
  let send_json_ (self : t) (j : json) : unit IO.t =
    let json = J.to_string j in
    Log.debug (fun k -> k "jsonrpc2: send json: %s" json);
    let full_s =
      Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length json) json
    in
    IO.write_string self.oc full_s

  let send_response (self : t) (m : Jsonrpc.Response.t) : unit IO.t =
    let json = Jsonrpc.Response.yojson_of_t m in
    send_json_ self json

  let send_server_notif (self : t) (m : Jsonrpc.Notification.t) : unit IO.t =
    let json = Jsonrpc.Notification.yojson_of_t m in
    send_json_ self json

  (** Send a server request to the LSP client. Invariant: you should call
      [register_server_request_response_handler] before calling this method to
      ensure that [handle_response] will have a registered handler for this
      response. *)
  let send_server_req (self : t) (m : Jsonrpc.Request.t) : unit IO.t =
    let json = Jsonrpc.Request.yojson_of_t m in
    send_json_ self json

  (** Returns a new, unused [Req_id.t] to send a server request. *)
  let fresh_lsp_id (self : t) : Req_id.t =
    let id = self.id_counter in
    self.id_counter <- id + 1;
    `Int id

  (** Registers a new handler for a request response. The return indicates
      whether a value was inserted or not (in which case it's already present). *)
  let register_server_request_response_handler (self : t) (id : Req_id.t)
      (handler : server_request_handler_pair) : bool =
    if Hashtbl.mem self.pending_responses id then
      false
    else (
      let () = Hashtbl.add self.pending_responses id handler in
      true
    )

  let try_ f =
    IO.catch
      (fun () ->
        let+ x = f () in
        Ok x)
      (fun e -> IO.return (Error e))

  (** Sends a server notification to the LSP client. *)
  let server_notification (self : t) (n : Lsp.Server_notification.t) : unit IO.t
      =
    let msg = Lsp.Server_notification.to_jsonrpc n in
    send_server_notif self msg

  (** Given a [server_request_handler_pair] consisting of some server request
      and its handler, sends this request to the LSP client and adds the handler
      to a table of pending responses. The request will later be handled by
      [handle_response], which will call the provided handler and delete it from
      the table of pending responses. *)
  let server_request (self : t) (req : server_request_handler_pair) :
      Req_id.t IO.t =
    let (Request_and_handler (r, _)) = req in
    let id = fresh_lsp_id self in
    let msg = Lsp.Server_request.to_jsonrpc_request r ~id in
    let has_inserted = register_server_request_response_handler self id req in
    if has_inserted then
      let* () = send_server_req self msg in
      return id
    else
      IO.failwith "failed to register server request: id was already used"

  (** Wraps some action and, in case the [IO.t] request has failed, logs the
      failure to the LSP client. *)
  let with_error_handler (self : t) (action : unit -> unit IO.t) : unit IO.t =
    IO.catch action (fun e ->
        let msg =
          Lsp.Types.LogMessageParams.create ~type_:Lsp.Types.MessageType.Error
            ~message:(Printexc.to_string e)
        in
        let msg =
          Lsp.Server_notification.LogMessage msg
          |> Lsp.Server_notification.to_jsonrpc
        in
        send_server_notif self msg)

  let handle_notification (self : t) (n : Jsonrpc.Notification.t) : unit IO.t =
    match Lsp.Client_notification.of_jsonrpc n with
    | Ok n ->
      with_error_handler self (fun () ->
          self.s#on_notification n ~notify_back:(server_notification self)
            ~server_request:(server_request self))
    | Error e -> IO.failwith (spf "cannot decode notification: %s" e)

  let handle_request (self : t) (r : Jsonrpc.Request.t) : unit IO.t =
    let protect ~id f =
      IO.catch f (fun e ->
          let message =
            spf "%s\n%s" (Printexc.to_string e) (Printexc.get_backtrace ())
          in
          Log.err (fun k -> k "error: %s" message);
          let r =
            Jsonrpc.Response.error id
              (Jsonrpc.Response.Error.make
                 ~code:Jsonrpc.Response.Error.Code.InternalError ~message ())
          in
          send_response self r)
    in
    (* request, so we need to reply *)
    let id = r.id in
    IO.catch
      (fun () ->
        match Lsp.Client_request.of_jsonrpc r with
        | Ok (Lsp.Client_request.E r) ->
          protect ~id (fun () ->
              let* reply =
                self.s#on_request r ~id ~notify_back:(server_notification self)
                  ~server_request:(server_request self)
              in
              let reply_json = Lsp.Client_request.yojson_of_result r reply in
              let response = Jsonrpc.Response.ok id reply_json in
              send_response self response)
        | Error e -> IO.failwith (spf "cannot decode request: %s" e))
      (fun e ->
        let message =
          spf "%s\n%s" (Printexc.to_string e) (Printexc.get_backtrace ())
        in
        Log.err (fun k -> k "error: %s" message);
        let r =
          Jsonrpc.Response.error id
            (Jsonrpc.Response.Error.make
               ~code:Jsonrpc.Response.Error.Code.InternalError ~message ())
        in
        send_response self r)

  let handle_response (self : t) (r : Jsonrpc.Response.t) : unit IO.t =
    match Hashtbl.find_opt self.pending_responses r.id with
    | None ->
      IO.failwith
      @@ Printf.sprintf "server request not found for response of id %s"
      @@ Req_id.to_string r.id
    | Some (Request_and_handler (req, handler)) ->
      let () = Hashtbl.remove self.pending_responses r.id in
      (match r.result with
      | Error err -> with_error_handler self (fun () -> handler @@ Error err)
      | Ok json ->
        let r = Lsp.Server_request.response_of_json req json in
        with_error_handler self (fun () -> handler @@ Ok r))

  let handle_batch_response (_self : t) (_rs : Jsonrpc.Response.t list) :
      unit IO.t =
    IO.failwith "Unhandled: jsonrpc batch response"

  let handle_batch_call (_self : t)
      (_cs :
        [ `Notification of Jsonrpc.Notification.t
        | `Request of Jsonrpc.Request.t
        ]
        list) : unit IO.t =
    IO.failwith "Unhandled: jsonrpc batch call"

  (* read a full message *)
  let read_msg (self : t) : (Jsonrpc.Packet.t, exn) result IO.t =
    let rec read_headers acc =
      let*? line = try_ @@ fun () -> IO.read_line self.ic in
      match String.trim line with
      | "" -> IO.return (Ok acc) (* last separator *)
      | line ->
        (match
           let i = String.index line ':' in
           if i < 0 || String.get line (i + 1) <> ' ' then raise Not_found;
           let key = String.lowercase_ascii @@ String.sub line 0 i in
           let v =
             String.lowercase_ascii
             @@ String.trim
                  (String.sub line (i + 1) (String.length line - i - 1))
           in
           key, v
         with
        | pair -> read_headers (pair :: acc)
        | exception _ ->
          IO.return
            (Error (E (ErrorCode.ParseError, spf "invalid header: %S" line))))
    in
    let*? headers = read_headers [] in
    Log.debug (fun k ->
        k "jsonrpc2: read headers: [%s]"
          (String.concat ";"
          @@ List.map (fun (a, b) -> Printf.sprintf "(%S,%S)" a b) headers));
    let ok =
      match List.assoc "content-type" headers with
      | "utf8" | "utf-8" -> true
      | _ -> false
      | exception Not_found -> true
    in
    if ok then (
      match int_of_string (List.assoc "content-length" headers) with
      | n ->
        Log.debug (fun k -> k "jsonrpc2: read %d bytes..." n);
        let buf = Bytes.make n '\000' in
        let*? () = try_ @@ fun () -> IO.read self.ic buf 0 n in
        (* log_lsp_ "got bytes %S" (Bytes.unsafe_to_string buf); *)
        let*? j =
          try_ @@ fun () ->
          IO.return @@ J.from_string (Bytes.unsafe_to_string buf)
        in
        Log.debug (fun k -> k "got json %s" (J.to_string j));
        (match Jsonrpc.Packet.t_of_yojson j with
        | m -> IO.return @@ Ok m
        | exception exn ->
          Log.err (fun k ->
              k "cannot decode json message: %s" (Printexc.to_string exn));
          IO.return (Error (E (ErrorCode.ParseError, "cannot decode json"))))
      | exception _ ->
        IO.return
        @@ Error (E (ErrorCode.ParseError, "missing content-length' header"))
    ) else
      IO.return
      @@ Error (E (ErrorCode.InvalidRequest, "content-type must be 'utf-8'"))

  let run ?(shutdown = fun _ -> false) (self : t) : unit IO.t =
    let process_msg r =
      let module M = Jsonrpc.Packet in
      match r with
      | M.Notification n -> handle_notification self n
      | M.Request r -> handle_request self r
      | M.Response r -> handle_response self r
      | M.Batch_response rs -> handle_batch_response self rs
      | M.Batch_call cs -> handle_batch_call self cs
    in
    let rec loop () =
      if shutdown () then
        IO.return ()
      else
        let* r = read_msg self in
        match r with
        | Ok r ->
          IO.spawn (fun () -> process_msg r);
          loop ()
        | Error e -> IO.fail e
    in
    loop ()
end
