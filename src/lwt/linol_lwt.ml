
module type IO = Linol.IO
module Make = Linol.Make
module Jsonrpc2 = Jsonrpc2
module Task = Task

include Lsp.Types
type doc_state = Jsonrpc2.doc_state
