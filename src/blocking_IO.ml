open Common_

type 'a t = 'a
type env = unit
type nonrec in_channel = in_channel
type nonrec out_channel = out_channel

let ( let+ ) x f = f x
let ( let* ) x f = f x
let ( and+ ) a b = a, b
let return x = x
let failwith = failwith
let fail = Printexc.raise_with_backtrace
let stdin () = stdin
let stdout () = stdout

let default_spawn f =
  let run () =
    let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "linol.spawn" in
    try f ()
    with e ->
      Log.err (fun k ->
          k "uncaught exception in `spawn`:\n%s\n%!" (Printexc.to_string e));
      raise e
  in
  ignore (Thread.create run ())

let catch f g =
  try f ()
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    g e bt

let n_bytes_written = Atomic.make 0
let n_bytes_read = Atomic.make 0

let rec read ic buf i len =
  if len > 0 then (
    let n = input ic buf i len in
    ignore (Atomic.fetch_and_add n_bytes_read n : int);
    read ic buf (i + n) (len - n)
  )

let read_line ic =
  let l = input_line ic in
  ignore (Atomic.fetch_and_add n_bytes_read (String.length l) : int);
  l

let write oc b i len =
  output oc b i len;
  ignore (Atomic.fetch_and_add n_bytes_written len : int);
  flush oc

let write_string oc s =
  output_string oc s;
  ignore (Atomic.fetch_and_add n_bytes_written (String.length s) : int);
  flush oc
