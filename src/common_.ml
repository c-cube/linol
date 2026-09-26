module Lsp = Linol_lsp.Lsp
module Jsonrpc = Linol_jsonrpc.Jsonrpc
module Trace = Trace_core
module ErrorCode = Jsonrpc.Response.Error.Code

exception E of ErrorCode.t * string

let ( let@ ) = ( @@ )
let spf = Printf.sprintf
