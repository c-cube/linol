
# 0.10

- use `git subtree` to vendor lsp+jsonrpc, so that they
    are not dependencies anymore and do not conflict with
    other users
- Add `filter_text_document` to ignore some documents

# 0.9

- Drop redundant dependency on atomic
- Add support for lsp 1.22

# 0.8

- move to LSP 1.19 and 1.20

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
