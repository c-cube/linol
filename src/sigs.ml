(** {2 Parametrized IO Interface} *)
module type IO = sig
  type 'a t

  val return : 'a -> 'a t
  val failwith : string -> 'a t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  type env
  type in_channel
  type out_channel

  val stdin : env -> in_channel
  val stdout : env -> out_channel
  val read : in_channel -> bytes -> int -> int -> unit t
  val read_line : in_channel -> string t
  val write : out_channel -> bytes -> int -> int -> unit t
  val write_string : out_channel -> string -> unit t
  val fail : exn -> Printexc.raw_backtrace -> unit t
  val catch : (unit -> 'a t) -> (exn -> Printexc.raw_backtrace -> 'a t) -> 'a t
end
