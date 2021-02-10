
module Fmt = CCFormat

type cancel = Lwt_switch.t

type 'a m = 'a Lwt.t
type 'a t = {
  descr: string option;
  cancel: cancel option;
  mutable n_child: int;
  cond: unit Lwt_condition.t;
  parent: parent;
  run: 'a t -> 'a m;
}
and parent =
  | NoParent
  | Parent : 'a t -> parent

let cancel self =
  let cancel_ s = Lwt.async (fun () -> Lwt_switch.turn_off s) in
  CCOpt.iter cancel_ self.cancel
let is_cancelled self = CCOpt.exists Lwt_switch.is_on self.cancel

let return x : _ m = Lwt.return x

let descr self = self.descr
let pause (self:_ t) =
  Lwt_switch.check self.cancel;
  Lwt.pause()

module Infix = struct
  let (>|=) = Lwt.(>|=)
  let (>>= ) = Lwt.(>>=)
  let (let+) = (>|=)
  let (let* ) = (>>=)
  let (and+ ) = Lwt.both
end

include Infix

let wait_all l =
  let+ l = Lwt.all l in
  match CCList.find_map (function Error e -> Some e | Ok () -> None) l with
  | None -> Ok ()
  | Some e -> Error e

let unwrap = function
  | Ok x -> return x
  | Error e -> Lwt.fail e

(** An exception caught and re-launched from a task *)
module Wrapped_error = struct
  type 'a task = 'a t

  type t = E : {
    task: 'a task;
    e: exn;
  } -> t

  exception Wrapped of t

  let rec pp out (E {task;e}) =
    let descr = CCOpt.get_or ~default:"<no descr>" task.descr in
    let pp_e out e = match e with
      | Wrapped e -> pp out e
      | e -> Fmt.string out (Printexc.to_string e)
    in
    Fmt.fprintf out "@[<v>error in task '%s':@ %a@]" descr pp_e e
  let to_string = Fmt.to_string pp
end

let run_ (self:'a t) : ('a, exn) result m =
  let rec wait_children() : unit m =
    if self.n_child = 0 then Lwt.return ()
    else (
      let* () = Lwt_condition.wait self.cond in
      wait_children()
    )
  in
  let res () =
    Lwt.catch
      (fun () ->
        let* x = self.run self in
        let+ () = wait_children() in
        Ok x)
      (fun e ->
         return @@ Error (Wrapped_error.Wrapped (Wrapped_error.E {task=self; e})))
  in
  match self.parent with
  | NoParent -> res()
  | Parent p ->
    p.n_child <- 1 + p.n_child;
    let+ r = res() in
    p.n_child <- p.n_child - 1;
    Lwt_condition.signal p.cond ();
    r

let run self = Lwt_main.run (run_ self)

let start ?descr ?cancel run : _ t =
  {descr; cancel; run; parent=NoParent; cond=Lwt_condition.create (); n_child=0; }

let run_sub ~parent ?descr ?cancel run : _ m =
  let t = {
    descr; cancel; run; parent=Parent parent;
    cond=Lwt_condition.create (); n_child=0;
  } in
  run_ t


let () =
  Printexc.register_printer
    (function
      | Wrapped_error.Wrapped e ->
        let s = Wrapped_error.to_string e in
        Some s
      | _ -> None)

