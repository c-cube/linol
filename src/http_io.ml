open Common_
module J = Yojson.Safe

module Make (IO : Sigs.StringIO) :
  Sigs.IO
    with type 'a t = 'a IO.t
     and type in_channel = IO.in_channel
     and type out_channel = IO.out_channel = struct
  include IO

  (* bind on IO+result *)
  let ( let*? ) x f =
    let* x = x in
    match x with
    | Ok x -> f x
    | Error _ as err -> IO.return err

  let try_ f =
    IO.catch
      (fun () ->
        let+ x = f () in
        Ok x)
      (fun e bt -> IO.return (Error (e, bt)))

  let send_msg oc ~json =
    let json = J.to_string json in
    let full_s =
      Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length json) json
    in
    write_string oc full_s

  let read_msg ic =
    let rec read_headers acc =
      let*? line = try_ @@ fun () -> IO.read_line ic in
      match String.trim line with
      | "" -> IO.return (Ok acc) (* last separator *)
      | line ->
        (match
           let i = String.index line ':' in
           if i < 0 || String.get line (i + 1) <> ' ' then raise Not_found;
           let key = String.lowercase_ascii @@ String.sub line 0 i in
           let v =
             String.lowercase_ascii
             @@ String.trim
                  (String.sub line (i + 1) (String.length line - i - 1))
           in
           key, v
         with
        | pair -> read_headers (pair :: acc)
        | exception _ ->
          let bt = Printexc.get_raw_backtrace () in
          let exn = E (ErrorCode.ParseError, spf "invalid header: %S" line) in
          IO.return (Error (exn, bt)))
    in
    let*? headers = read_headers [] in
    Log.debug (fun k ->
        k "jsonrpc2: read headers: [%s]"
          (String.concat ";"
          @@ List.map (fun (a, b) -> Printf.sprintf "(%S,%S)" a b) headers));
    let ok =
      match List.assoc "content-type" headers with
      | "utf8" | "utf-8" -> true
      | _ -> false
      | exception Not_found -> true
    in
    if ok then (
      match int_of_string (List.assoc "content-length" headers) with
      | n ->
        Log.debug (fun k -> k "jsonrpc2: read %d bytes..." n);
        let buf = Bytes.make n '\000' in
        let*? () = try_ @@ fun () -> read ic buf 0 n in
        (* log_lsp_ "got bytes %S" (Bytes.unsafe_to_string buf); *)
        let*? j =
          Fun.id @@ try_
          @@ fun () -> IO.return @@ J.from_string (Bytes.unsafe_to_string buf)
        in
        IO.return (Ok j)
      | exception _ ->
        let bt = Printexc.get_raw_backtrace () in
        IO.return
        @@ Error (E (ErrorCode.ParseError, "missing content-length' header"), bt)
    ) else (
      let bt = Printexc.get_callstack 10 in
      IO.return
      @@ Error (E (ErrorCode.InvalidRequest, "content-type must be 'utf-8'"), bt)
    )
end
