
module type IO = Lsp_server.IO
module Make = Lsp_server.Make
module Jsonrpc2 = Jsonrpc2
module Task = Task

include Lsp.Types
type doc_state = Jsonrpc2.doc_state
