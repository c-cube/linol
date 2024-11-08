
# 0.7

- Handle `End_of_file` by exiting
- Fix: make server requests thread safe
- Use positionEncoding advertised by clients
- Use IO.catch in async
- Log exceptions in async notification handler
- Add more error handlers and logging around notif/request handlers
- Add on_req_shutdown

# 0.6

- Support textDocument/didSave notification
- advertise inlay hints server capability
- Implement Eio backend in `linol-eio`
- jsonrpc2: add ?on_received/?on_sent
- drop redundant dependency on atomic

# 0.5

- api break: put `spawn` in the server itself, not `IO`

- require OCaml 4.14
- migrate to lsp 1.17
- support inlay hints
- internal tracing with `trace`
- [#24] Expose get_uri for notify_back
- expose log source
- [#22] Threat shutdown and exit requests correctly
- [#20] Handle messages with null value for  "params" field
- Handle server requests
- handle workDoneTokens
