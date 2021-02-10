
(** {1 Tasks}

    Tasks built on top of Lwt, for cooperative multi-threading. *)

type 'a t
(** A task *)

type 'a m = 'a Lwt.t
(** Computation within the task *)

type cancel

val return : 'a -> 'a m

val start :
  ?descr:string ->
  ?cancel:cancel -> ('a t -> 'a m) -> 'a t

val descr : _ t -> string option

val run : 'a t -> ('a, exn) result

val run_sub :
  parent:_ t ->
  ?descr:string ->
  ?cancel:cancel ->
  ('a t -> 'a m) -> ('a, exn) result m

module Wrapped_error : sig
  type 'a task = 'a t

  type t = E : {
    task: 'a task;
    e: exn;
  } -> t

  (** An exception caught and re-launched from a task *)
  exception Wrapped of t

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end

val unwrap : ('a, exn) result -> 'a m
val is_cancelled : _ t -> bool
val cancel : _ t -> unit
val pause : _ t -> unit m

val wait_all : (unit, exn) result m list -> (unit, exn) result m

module Infix : sig
  val (let+) : 'a m -> ('a -> 'b) -> 'b m
  val (let* ) : 'a m -> ('a -> 'b m) -> 'b m
  val (>|=) : 'a m -> ('a -> 'b) -> 'b m
  val (>>= ) : 'a m -> ('a -> 'b m) -> 'b m
  val (and+ ) : 'a m -> 'b m -> ('a*'b) m
end

include module type of Infix
