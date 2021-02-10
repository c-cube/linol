
module IO : Linol.IO
  with type 'a t = 'a Task.m
   and type in_channel = Lwt_io.input Lwt_io.channel
   and type out_channel = Lwt_io.output Lwt_io.channel

type json = Yojson.Safe.t

type t
(** A jsonrpc2 connection. *)

include module type of Linol.Make(IO)

val create :
  ic:IO.in_channel ->
  oc:IO.out_channel ->
  server ->
  t
(** Create a connection from the pair of channels *)

val create_stdio : server -> t

val run : t -> unit Task.t -> unit Task.m
(** Listen for incoming messages and responses *)
