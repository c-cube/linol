(** Linol.

    Abstraction over The "Lsp" library, to make it easier to develop LSP servers
    in OCaml (but not necessarily {b for} OCaml). *)

module type IO = Sigs.IO

module Jsonrpc2 = Jsonrpc2
module Server = Server
module Blocking_IO = Blocking_IO
module Log = Log
module Make = Jsonrpc2.Make

let logs_src = Log.src
