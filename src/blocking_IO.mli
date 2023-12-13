(** Blocking IO with a new thread for each [spawn]. *)

include
  Sigs.IO
    with type 'a t = 'a
     and type in_channel = in_channel
     and type out_channel = out_channel

val n_bytes_written : int Atomic.t
(** @since NEXT_RELEASE *)

val n_bytes_read : int Atomic.t
(** @since NEXT_RELEASE *)

val default_spawn : (unit -> unit) -> unit
(** Start a new thread.
    @since NEXT_RELEASE *)
