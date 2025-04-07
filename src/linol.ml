(** Linol.

    Abstraction over The "Lsp" library, to make it easier to develop
    LSP servers in OCaml (but not necessarily {b for} OCaml). *)

module type IO = Sigs.IO

(** {2 Re-export from vendored lsp} *)

module Lsp = Linol_lsp.Lsp
module Jsonrpc = Linol_jsonrpc.Jsonrpc

(** {2 Main modules} *)

module Jsonrpc2 = Jsonrpc2
module Server = Server
module Blocking_IO = Blocking_IO
module Log = Log
module Make = Jsonrpc2.Make

let logs_src = Log.src
