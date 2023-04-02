(** Simple JSON-RPC2 implementation.

    See {{: https://www.jsonrpc.org/specification} the spec} *)

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
  (** Listen for incoming messages and responses.
    @param shutdown if true, tells the server to shut down *)
end

module Make (IO : IO) : S with module IO = IO
