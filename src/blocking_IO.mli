(** {1 Blocking IO with a new thread for each [spawn]} *)

include
  Sigs.IO
    with type 'a t = 'a
     and type in_channel = in_channel
     and type out_channel = out_channel

val set_spawn_function : ((unit -> unit) -> unit) -> unit
(** Change the way the LSP server spawns new threads to handle
    client queries. For example, one might use a thread pool. *)
