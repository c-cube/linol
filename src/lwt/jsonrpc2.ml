
(** {1 Simple JSON-RPC2 implementation}
    See {{: https://www.jsonrpc.org/specification} the spec} *)

module Fmt = CCFormat
module J = Yojson.Safe
module Err = Jsonrpc.Response.Error
open Task.Infix

module IO = struct
  type 'a t = 'a Lwt.t
  let (let+) = Lwt.(>|=)
  let (let*) = Lwt.(>>=)
  let (and+) a b =
    let open Lwt in
    a >>= fun x -> b >|= fun y -> x,y
  let return = Lwt.return
  let failwith = Lwt.fail_with
  type in_channel = Lwt_io.input Lwt_io.channel
  type out_channel = Lwt_io.output Lwt_io.channel
end

include Linol.Make(IO)

type json = J.t
type 'a m = 'a Task.m

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

type t = {
  ic: Lwt_io.input Lwt_io.channel;
  oc: Lwt_io.output Lwt_io.channel;
  s: server;
}

let create ~ic ~oc server : t = {ic; oc; s=server}

let create_stdio server : t =
  create ~ic:Lwt_io.stdin ~oc:Lwt_io.stdout server

(* bind on IO+result *)
let ( let*? ) x f =
  let open Lwt.Infix in
  x >>= function
  | Error _ as err -> Lwt.return err
  | Ok x -> f x

(* send a single message *)
let send_json_ (self:t) (j:json) : unit m =
  let json = J.to_string j in
  let full_s =
    Printf.sprintf "Content-Length: %d\r\n\r\n%s"
      (String.length json) json
  in
  Lwt_io.write self.oc full_s

let send_response (self:t) (m:Jsonrpc.Response.t) : unit m =
  let json = Jsonrpc.Response.yojson_of_t m in
  send_json_ self json

let send_server_notif (self:t) (m:Jsonrpc.Message.notification) : unit m =
  let json = Jsonrpc.Message.yojson_of_notification m in
  send_json_ self json

let try_ f =
  Lwt.catch
    (fun () -> let+ x = f() in Ok x)
    (fun e -> Lwt.return (Error e))

let log_lsp_ msg =
  Fmt.kasprintf
    (fun s ->
      Lsp.Logger.log ~title:Lsp.Logger.Title.Debug ~section:"jsonrpc2"
      "%s" s)
    msg

(* read a full message *)
let read_msg (self:t) : (Jsonrpc.Message.either, exn) result m =
  let rec read_headers acc =
    let*? line =
      try_ @@ fun () -> Lwt_io.read_line self.ic
    in
    match String.trim line with
    | "" -> Lwt.return (Ok acc) (* last separator *)
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
          Lwt.return (Error (E(ErrorCode.ParseError, spf "invalid header: %S" line)))
      end
  in
  let*? headers = read_headers [] in
  log_lsp_ "headers: %a" Fmt.Dump.(list @@ pair string string) headers;
  let ok = match List.assoc "content-type" headers with
    | "utf8" | "utf-8" -> true
    | _ -> false
    | exception Not_found -> true
  in
  if ok then (
    match int_of_string (List.assoc "content-length" headers) with
    | n ->
      log_lsp_ "read %d bytes..." n;
      let buf = Bytes.make n '\000' in
      let*? () =
        try_ @@ fun () -> Lwt_io.read_into_exactly self.ic buf 0 n
      in
      (* log_lsp_ "got bytes %S" (Bytes.unsafe_to_string buf); *)
      let*? j =
        try_ @@ fun () ->
        Lwt.return @@ J.from_string (Bytes.unsafe_to_string buf)
      in
      begin match Jsonrpc.Message.either_of_yojson j with
        | m -> Lwt.return @@ Ok m
        | exception _ ->
          Lwt.return (Error (E(ErrorCode.ParseError, "cannot decode json")))
      end
    | exception _ ->
      Lwt.return @@
      Error (E(ErrorCode.ParseError, "missing content-length' header"))
  ) else (
    Lwt.return @@
    Error (E(ErrorCode.InvalidRequest, "content-type must be 'utf-8'"))
  )

let run (self:t) (task:_ Task.t) : unit m =
  let process_msg r =
    let module M = Jsonrpc.Message in
    let protect ~id f =
      Lwt.catch f
        (fun e ->
           let r = Jsonrpc.Response.error id
             (Jsonrpc.Response.Error.make
               ~code:Jsonrpc.Response.Error.Code.InternalError
               ~message:(Printexc.to_string e) ())
          in
          send_response self r)
    in
    match r.M.id with
    | None ->
      (* notification *)
      begin match Lsp.Client_notification.of_jsonrpc {r with M.id=()} with
        | Ok n ->
          Lwt.catch
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
          Lwt.fail_with (spf "cannot decode notification: %s" e)
      end
    | Some id ->
      (* request, so we need to reply *)
      Lwt.catch
        (fun () ->
          begin match Lsp.Client_request.of_jsonrpc {r with M.id} with
            | Ok (Lsp.Client_request.E r) ->
              protect ~id (fun () ->
                let* reply = self.s#on_request r in
                let reply_json = Lsp.Client_request.yojson_of_result r reply in
                let response = Jsonrpc.Response.ok id reply_json in
                send_response self response
              )
            | Error e ->
              Lwt.fail_with (spf "cannot decode request: %s" e)
          end)
        (fun e ->
          let r =
            Jsonrpc.Response.error id
            (Jsonrpc.Response.Error.make
              ~code:Jsonrpc.Response.Error.Code.InternalError
              ~message:(Printexc.to_string e) ())
          in
          send_response self r)
  in
  let rec loop () =
    if Task.is_cancelled task then Lwt.return ()
    else (
      let* r = read_msg self >>= Task.unwrap in
      Lwt.async (fun () -> process_msg r);
      loop()
    )
  in
  loop()

