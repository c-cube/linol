module type IO = Linol.IO

module IO_lwt :
  IO
    with type 'a t = 'a Lwt.t
     and type in_channel = Lwt_io.input Lwt_io.channel
     and type out_channel = Lwt_io.output Lwt_io.channel = struct
  type 'a t = 'a Lwt.t

  let ( let+ ) = Lwt.( >|= )
  let ( let* ) = Lwt.( >>= )

  let ( and+ ) a b =
    let open Lwt in
    a >>= fun x ->
    b >|= fun y -> x, y

  let return = Lwt.return
  let failwith = Lwt.fail_with
  let stdin = Lwt_io.stdin
  let stdout = Lwt_io.stdout

  type in_channel = Lwt_io.input Lwt_io.channel
  type out_channel = Lwt_io.output Lwt_io.channel

  let write_string = Lwt_io.write
  let write = Lwt_io.write_from_exactly
  let read = Lwt_io.read_into_exactly
  let read_line = Lwt_io.read_line

  let catch f g =
    let bt = Printexc.get_callstack 10 in
    Lwt.catch f (fun exn -> g exn bt)

  let fail e _bt = Lwt.fail e
end

(** Spawn function.
    @since 0.5 *)
let spawn f =
  Lwt.async (fun () ->
      Lwt.catch f (fun exn ->
          Printf.eprintf "uncaught exception in `spawn`:\n%s\n%!"
            (Printexc.to_string exn);
          Lwt.return ()))

include Lsp.Types
include IO_lwt

type doc_state = Linol.Server.doc_state

module Jsonrpc2 = Linol.Jsonrpc2.Make (IO_lwt)

let run = Lwt_main.run
