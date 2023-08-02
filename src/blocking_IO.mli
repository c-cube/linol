(** Blocking IO with a new thread for each [spawn]. *)

include
  Sigs.IO
    with type 'a t = 'a
     and type in_channel = in_channel
     and type out_channel = out_channel

val default_spawn : (unit -> unit) -> unit
(** Start a new thread.
    @since NEXT_RELEASE *)
