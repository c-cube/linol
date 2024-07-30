module type IO = Linol.IO

module IO_eio :
  IO
    with type 'a t = 'a
     and type env = Eio_unix.Stdenv.base
     and type in_channel = Eio.Buf_read.t
     and type out_channel = Eio_unix.sink_ty Eio.Std.r = struct
  type 'a t = 'a

  let ( let+ ) x f = f x
  let ( let* ) x f = f x
  let ( and+ ) a b = a, b
  let return x = x
  let failwith = failwith
  let fail = raise

  let catch f handler = try f () with exn ->
    let bt = Printexc.get_raw_backtrace () in
    handler exn bt

  let stdin env = Eio.Buf_read.of_flow ~max_size:1_000_000 (Eio.Stdenv.stdin env)
  let stdout = Eio.Stdenv.stdout

  type env = Eio_unix.Stdenv.base
  type in_channel = Eio.Buf_read.t
  type out_channel = Eio_unix.sink_ty Eio.Std.r

  let write_string out_ch str = Eio.Flow.copy_string str out_ch
  let write out_ch bytes off len =
    Eio.Buf_write.with_flow out_ch @@ fun w ->
    Eio.Buf_write.bytes w ~off ~len bytes
  let read in_ch bytes off len =
    let str = Eio.Buf_read.take len in_ch in
    Bytes.blit_string str off bytes 0 len
  let read_line in_ch =
    Eio.Buf_read.line in_ch
end

(** Spawn function. *)
let spawn (f:unit -> (unit, string) result) : unit  =
  let promise, resolver = Eio.Promise.create () in
  begin
    try
      match f () with
      | Ok _ -> Eio.Promise.resolve_ok resolver ()
      | Error _ -> ()
    with
      exn ->
      (Printf.eprintf "uncaught exception in `spawn`:\n%s\n%!"
        (Printexc.to_string exn));
      Eio.Promise.resolve_error resolver exn
    end;

  (Eio.Promise.await_exn promise)

include Lsp.Types
include IO_eio

type doc_state = Linol.Server.doc_state

module Jsonrpc2 = Linol.Jsonrpc2.Make (IO_eio)
