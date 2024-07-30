(** Simple JSON-RPC2 implementation.

    See {{: https://www.jsonrpc.org/specification} the spec} *)

type json = Yojson.Safe.t

module type IO = Sigs.IO

module type S = sig
  module IO : IO

  type t
  (** A jsonrpc2 connection. *)

  include module type of Server.Make (IO)

  val create :
    ?on_received:(json -> unit) ->
    ?on_sent:(json -> unit) ->
    ic:IO.in_channel ->
    oc:IO.out_channel ->
    server ->
    t
  (** Create a connection from the pair of channels *)

  val create_stdio :
    ?on_received:(json -> unit) -> ?on_sent:(json -> unit) -> env:IO.env -> server -> t
  (** Create a connection using stdin/stdout *)

  val send_server_notification : t -> Lsp.Server_notification.t -> (unit, string) result IO.t
  (** Send a notification from the server.
      @since 0.5 *)

  val send_server_request :
    t ->
    'from_server Lsp.Server_request.t ->
    (('from_server, Jsonrpc.Response.Error.t) result -> unit IO.t) ->
    Req_id.t IO.t
  (** Send a request from the server, and pass a callback that will be
      called with the result in the future.
      @since 0.5 *)

  val run : ?shutdown:(unit -> bool) -> t -> unit IO.t
  (** Listen for incoming messages and responses.
    @param shutdown if true, tells the server to shut down *)
end

module Make (IO : IO) : S with module IO = IO
