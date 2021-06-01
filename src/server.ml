
open Sigs

(** Current state of a document. *)
type nonrec doc_state = {
  uri: Lsp.Types.DocumentUri.t;
  languageId: string;
  version: int;
  content: string;
}

(** {2 Request ID}

    unique ID of a request, used by JSONRPC to map each request to its reply. *)
module Req_id = struct
  type t = Jsonrpc.Id.t

  (** String representation of the ID *)
  let to_string : t -> string = function
    | `String s -> s
    | `Int i -> string_of_int i
end

(** {2 Server interface for some IO substrate} *)
module Make(IO : IO) = struct
  open Lsp.Types

  module Position = Position
  module Range = Range
  module Diagnostic = Diagnostic
  module DiagnosticSeverity = DiagnosticSeverity
  module Req_id = Req_id

  (** The server baseclass *)
  class virtual base_server = object
    method virtual on_notification :
      notify_back:(Lsp.Server_notification.t -> unit IO.t) ->
      Lsp.Client_notification.t ->
      unit IO.t

    method virtual on_request : 'a.
      notify_back:(Lsp.Server_notification.t -> unit IO.t) ->
      id:Req_id.t ->
      'a Lsp.Client_request.t ->
      'a IO.t
    (** Method called to handle client requests.
        @param notify_back an object used to reply to the client, send progress
        messages, diagnostics, etc.
        @param id the query RPC ID, can be used for tracing, cancellation, etc. *)

    (** Set to true if the client requested to exit *)
    method must_quit = false
  end

  (** A wrapper to more easily reply to notifications *)
  class notify_back ~workDoneToken ~notify_back
      ?version ?(uri:DocumentUri.t option) () =
    let send_work_progress = match workDoneToken with
      | None -> fun _ -> IO.return ()
      | Some token ->
        fun value ->
        let params = Lsp.Types.ProgressParams.create ~token ~value in
        notify_back (Lsp.Server_notification.WorkDoneProgress params)
    in
  object
    val mutable uri = uri
    method set_uri u = uri <- Some u

    (** Send a log message to the editor *)
    method send_log_msg ~type_ msg : unit IO.t =
      let params = LogMessageParams.create ~type_ ~message:msg in
      notify_back (Lsp.Server_notification.LogMessage params)

    (** Report beginning of progress for background work.
        @since NEXT_RELEASE *)
    method work_progress_begin ?percentage ~title ~message () : unit IO.t =
      let progress = WorkDoneProgressBegin.create ~title ~message ?percentage () in
      let value = Lsp.Server_notification.Progress.Begin progress in
      send_work_progress value

    (** Report progress.
        @since NEXT_RELEASE *)
    method work_progress_report ?percentage ~message () : unit IO.t =
      let progress = WorkDoneProgressReport.create ~message ?percentage () in
      let value = Lsp.Server_notification.Progress.Report progress in
      send_work_progress value

    (** Report end of progress for background work.
        @since NEXT_RELEASE *)
    method work_progress_end ~message () : unit IO.t =
      let progress = WorkDoneProgressEnd.create ~message () in
      let value = Lsp.Server_notification.Progress.End progress in
      send_work_progress value

    (** Send diagnostics for the current document *)
    method send_diagnostic (l:Diagnostic.t list) : unit IO.t =
      match uri with
      | None -> IO.failwith "notify_back: cannot publish diagnostics, no URI given"
      | Some uri ->
        let params = PublishDiagnosticsParams.create
            ~uri ?version ~diagnostics:l () in
        notify_back (Lsp.Server_notification.PublishDiagnostics params)

    (** Send a notification (general purpose method) *)
    method send_notification (n:Lsp.Server_notification.t) =
      notify_back n
  end

  (** Current state of a document. *)
  type nonrec doc_state = doc_state = {
    uri: DocumentUri.t;
    languageId: string;
    version: int;
    content: string;
  }

  (** An easily overloadable class. Pick the methods you want to support.
      The user must provide at least the callbacks for document lifecycle:
      open, close, update. The most basic LSP server should check documents
      when they're updated and report diagnostics back to the editor. *)
  class virtual server = object(self)
    inherit base_server
    val mutable _quit = false
    val docs : (DocumentUri.t, doc_state) Hashtbl.t = Hashtbl.create 16

    method! must_quit = _quit

    (** Find current state of the given document, if present. *)
    method find_doc (uri:DocumentUri.t) : doc_state option =
      try Some (Hashtbl.find docs uri)
      with Not_found -> None

    (** Override to process other requests *)
    method on_request_unhandled
      : type r. notify_back:notify_back ->
        id:Req_id.t -> r Lsp.Client_request.t -> r IO.t
      = fun ~notify_back:_ ~id:_ _r ->
        Log.debug (fun k->k "req: unhandled request");
        IO.failwith "TODO: handle this request"

    (** Parameter for how to synchronize content with the editor *)
    method config_sync_opts : TextDocumentSyncOptions.t =
      TextDocumentSyncOptions.create
          ~change:TextDocumentSyncKind.Incremental ~willSave:false ()

    method config_completion : CompletionOptions.t option = None
    (** Configuration for the completion API.
        @since 0.4 *)

    method config_code_lens_options : CodeLensOptions.t option = None
    (** @since 0.3 *)

    method config_definition :
      [`Bool of bool | `DefinitionOptions of DefinitionOptions.t ] option = None
    (** @since 0.3 *)

    method config_hover :
      [`Bool of bool | `HoverOptions of HoverOptions.t ] option = None
    (** @since 0.3 *)

    method config_symbol :
      [`Bool of bool | `DocumentSymbolOptions of DocumentSymbolOptions.t ] option = None
    (** @since 0.3 *)

    method config_code_action_provider :
      [`CodeActionOptions of CodeActionOptions.t | `Bool of bool] = `Bool false
    (** @since 0.3 *)

    (** Modify capabilities before sending them back to the client.
        By default we just return them unmodified.
        @since 0.3 *)
    method config_modify_capabilities (c:ServerCapabilities.t) : ServerCapabilities.t = c

    (** List of commands available *)
    method config_list_commands : string list = []

    method on_req_initialize ~notify_back:_
        (_i:InitializeParams.t) : InitializeResult.t IO.t =
      let sync_opts = self#config_sync_opts in
      let capabilities =
        ServerCapabilities.create
          ?codeLensProvider:self#config_code_lens_options
          ~codeActionProvider:self#config_code_action_provider
          ~executeCommandProvider:(ExecuteCommandOptions.create
                                     ~commands:self#config_list_commands ())
          ?completionProvider:self#config_completion
          ?definitionProvider:self#config_definition
          ?hoverProvider:self#config_hover
          ?documentSymbolProvider:self#config_symbol
          ~textDocumentSync:(`TextDocumentSyncOptions sync_opts) ()
        |> self#config_modify_capabilities
      in
      IO.return @@ InitializeResult.create ~capabilities ()

    (** Called when the user hovers on some identifier in the document *)
    method on_req_hover ~notify_back:_ ~id:_ ~uri:_ ~pos:_
        (_ : doc_state) : Hover.t option IO.t =
      IO.return None

    (** Called when the user requests completion in the document *)
    method on_req_completion  ~notify_back:_ ~id:_ ~uri:_ ~pos:_ ~ctx:_
        (_ : doc_state) :
          [ `CompletionList of CompletionList.t
          | `List of CompletionItem.t list ] option IO.t =
      IO.return None

    (** Called when the user wants to jump-to-definition  *)
    method on_req_definition  ~notify_back:_ ~id:_ ~uri:_ ~pos:_
        (_ : doc_state) : Locations.t option IO.t =
      IO.return None

    (** List code lenses for the given document
        @since 0.3 *)
    method on_req_code_lens  ~notify_back:_ ~id:_ ~uri:_
        (_ : doc_state) : CodeLens.t list IO.t =
      IO.return []

    (** Code lens resolution, must return a code lens with non null "command"
        @since 0.3 *)
    method on_req_code_lens_resolve
        ~notify_back:(_:notify_back) ~id:_ (cl:CodeLens.t) : CodeLens.t IO.t =
      IO.return cl

    (** Code action.
        @since 0.3 *)
    method on_req_code_action ~notify_back:(_:notify_back) ~id:_ (_c:CodeActionParams.t)
      : CodeActionResult.t IO.t =
      IO.return None

    (** Execute a command with given arguments.
        @since 0.3 *)
    method on_req_execute_command ~notify_back:_ ~id:_
        (_c:string) (_args:Yojson.Safe.t list option) : Yojson.Safe.t IO.t =
      IO.return `Null

    (** List symbols in this document.
        @since 0.3 *)
    method on_req_symbol ~notify_back:_ ~id:_ ~uri:_
        () : [ `DocumentSymbol of DocumentSymbol.t list
             | `SymbolInformation of SymbolInformation.t list ] option IO.t =
      IO.return None

    method on_request
    : type r. notify_back:_ -> id:Req_id.t -> r Lsp.Client_request.t -> r IO.t
    = fun ~notify_back ~id (r:_ Lsp.Client_request.t) ->
      Log.debug (fun k->k "handle request[id=%s] <opaque>" (Req_id.to_string id));

      begin match r with
        | Lsp.Client_request.Shutdown ->
          Log.info (fun k->k "shutdown");
          _quit <- true; IO.return ()

        | Lsp.Client_request.Initialize i ->
          Log.debug (fun k->k "req: initialize");
          let notify_back = new notify_back ~notify_back () in
          self#on_req_initialize ~notify_back i

        | Lsp.Client_request.TextDocumentHover { textDocument; position; workDoneToken } ->
          let uri = textDocument.uri in
          Log.debug (fun k->k "req: hover '%s'" @@ DocumentUri.to_string uri);

          begin match Hashtbl.find_opt docs uri with
            | None -> IO.return None
            | Some doc_st ->
              let notify_back = new notify_back ~workDoneToken ~uri ~notify_back () in
              self#on_req_hover ~notify_back ~id ~uri ~pos:position doc_st
          end

        | Lsp.Client_request.TextDocumentCompletion {
            textDocument; position; context; workDoneToken; partialResultToken=_ } ->
          let uri = textDocument.uri in
          Log.debug (fun k->k "req: complete '%s'" @@ DocumentUri.to_string uri);
          begin match Hashtbl.find_opt docs uri with
            | None -> IO.return None
            | Some doc_st ->
              let notify_back = new notify_back ~workDoneToken ~uri ~notify_back () in
              self#on_req_completion ~notify_back ~id ~uri
                ~pos:position ~ctx:context doc_st
          end
        | Lsp.Client_request.TextDocumentDefinition {
            textDocument; position; workDoneToken; partialResultToken=_; } ->
          let uri = textDocument.uri in
          Log.debug (fun k->k "req: definition '%s'" @@ DocumentUri.to_string uri);
          let notify_back = new notify_back ~workDoneToken ~uri ~notify_back () in

          begin match Hashtbl.find_opt docs uri with
            | None -> IO.return None
            | Some doc_st ->
              self#on_req_definition ~notify_back ~id
                ~uri ~pos:position doc_st
          end

        | Lsp.Client_request.TextDocumentCodeLens
            {textDocument; workDoneToken; partialResultToken=_; } ->
          let uri = textDocument.uri in
          Log.debug (fun k->k "req: codelens '%s'" @@ DocumentUri.to_string uri);
          let notify_back = new notify_back ~uri ~notify_back ~workDoneToken () in

          begin match Hashtbl.find_opt docs uri with
            | None -> IO.return []
            | Some doc_st ->
              self#on_req_code_lens ~notify_back ~id ~uri doc_st
          end

        | Lsp.Client_request.TextDocumentCodeLensResolve cl ->
          Log.debug (fun k->k "req: codelens resolve");
          let notify_back = new notify_back ~notify_back ~workDoneToken:None () in
          self#on_req_code_lens_resolve ~notify_back ~id cl

        | Lsp.Client_request.ExecuteCommand { command; arguments; workDoneToken; } ->
          Log.debug (fun k->k "req: execute command '%s'" command);
          let notify_back = new notify_back ~notify_back ~workDoneToken () in
          self#on_req_execute_command ~notify_back ~id command arguments

        | Lsp.Client_request.DocumentSymbol
            { textDocument=d; workDoneToken; partialResultToken=_ } ->
          let notify_back = new notify_back ~notify_back ~workDoneToken () in
          self#on_req_symbol ~notify_back ~id ~uri:d.uri ()

        | Lsp.Client_request.CodeAction a ->
          let notify_back = new notify_back ~notify_back ~workDoneToken:None () in
          self#on_req_code_action ~notify_back ~id a

        | Lsp.Client_request.TextDocumentDeclaration _
        | Lsp.Client_request.TextDocumentTypeDefinition _
        | Lsp.Client_request.TextDocumentPrepareRename _
        | Lsp.Client_request.TextDocumentRename _
        | Lsp.Client_request.TextDocumentLink _
        | Lsp.Client_request.TextDocumentLinkResolve _
        | Lsp.Client_request.WorkspaceSymbol _
        | Lsp.Client_request.DebugEcho _
        | Lsp.Client_request.DebugTextDocumentGet _
        | Lsp.Client_request.TextDocumentReferences _
        | Lsp.Client_request.TextDocumentHighlight _
        | Lsp.Client_request.TextDocumentFoldingRange _
        | Lsp.Client_request.SignatureHelp _
        | Lsp.Client_request.CompletionItemResolve _
        | Lsp.Client_request.WillSaveWaitUntilTextDocument _
        | Lsp.Client_request.TextDocumentFormatting _
        | Lsp.Client_request.TextDocumentOnTypeFormatting _
        | Lsp.Client_request.TextDocumentColorPresentation _
        | Lsp.Client_request.TextDocumentColor _
        | Lsp.Client_request.SelectionRange _
        | Lsp.Client_request.UnknownRequest _
        | Lsp.Client_request.TextDocumentMoniker _
        | Lsp.Client_request.CodeActionResolve _
        | Lsp.Client_request.SemanticTokensDelta _
        | Lsp.Client_request.SemanticTokensFull _
        | Lsp.Client_request.SemanticTokensRange _
        | Lsp.Client_request.LinkedEditingRange _
          ->
          let notify_back = new notify_back ~notify_back ~workDoneToken:None () in
          self#on_request_unhandled ~notify_back ~id r
      end

    (** Called when a document is opened *)
    method virtual on_notif_doc_did_open :
      notify_back:notify_back ->
      TextDocumentItem.t ->
      content:string ->
      unit IO.t

    method virtual on_notif_doc_did_close :
      notify_back:notify_back ->
      TextDocumentIdentifier.t ->
      unit IO.t

    (** Called when the document changes. *)
    method virtual on_notif_doc_did_change :
      notify_back:notify_back ->
      VersionedTextDocumentIdentifier.t ->
      TextDocumentContentChangeEvent.t list ->
      old_content:string ->
      new_content:string ->
      unit IO.t

    (** Override to handle unprocessed notifications *)
    method on_notification_unhandled
        ~notify_back:_ (_n:Lsp.Client_notification.t) : unit IO.t =
      IO.return ()

    method on_notification
        ~notify_back (n:Lsp.Client_notification.t) : unit IO.t =
      let open Lsp.Types in

      Log.debug
        (fun k->k "handle notification: %a" Yojson.Safe.pp
            (Lsp.Client_notification.to_jsonrpc n |> Jsonrpc.Message.yojson_of_notification));

      begin match n with
        | Lsp.Client_notification.TextDocumentDidOpen
            {DidOpenTextDocumentParams.textDocument=doc} ->
          Log.debug (fun k->k "notif: did open '%s'" @@ DocumentUri.to_string doc.uri);
          let notify_back =
            new notify_back ~uri:doc.uri ~workDoneToken:None
              ~version:doc.version ~notify_back () in
          let st = {
            uri=doc.uri; version=doc.version; content=doc.text;
            languageId=doc.languageId;
          } in
          Hashtbl.replace docs doc.uri st;
          self#on_notif_doc_did_open ~notify_back doc ~content:st.content

        | Lsp.Client_notification.TextDocumentDidClose {textDocument=doc} ->
          Log.debug (fun k->k "notif: did close '%s'" @@ DocumentUri.to_string doc.uri);
          let notify_back = new notify_back ~uri:doc.uri ~workDoneToken:None ~notify_back () in
          self#on_notif_doc_did_close ~notify_back doc

        | Lsp.Client_notification.TextDocumentDidChange
            {textDocument=doc; contentChanges=c} ->
          Log.debug (fun k->k "notif: did change '%s'" @@ DocumentUri.to_string doc.uri);
          let notify_back =
            new notify_back ~uri:doc.uri ~workDoneToken:None ~notify_back () in

          let old_doc =
            match Hashtbl.find_opt docs doc.uri with
            | None ->
              (* WTF vscode. Well let's try and deal with it. *)
              Log.err (fun k->k "unknown document: '%s'" @@ DocumentUri.to_string doc.uri);
              let version = doc.version in

              let languageId = "" in (* FIXME*)
              Lsp.Text_document.make
                (DidOpenTextDocumentParams.create
                   ~textDocument:(
                     TextDocumentItem.create ~languageId
                       ~uri:doc.uri ~version ~text:""))
            | Some st ->
                Lsp.Text_document.make
                  (DidOpenTextDocumentParams.create
                     ~textDocument:(
                       TextDocumentItem.create ~languageId:st.languageId
                         ~uri:doc.uri ~version:st.version ~text:st.content))
          in

          let new_doc: Lsp.Text_document.t =
            List.fold_left
              (fun d ev -> Lsp.Text_document.apply_content_change d ev)
              old_doc c
          in

          let new_st : doc_state = {
            uri=doc.uri; languageId=Lsp.Text_document.languageId new_doc;
            content=Lsp.Text_document.text new_doc;
            version=Lsp.Text_document.version new_doc;
          } in

          Hashtbl.replace docs doc.uri new_st;
          self#on_notif_doc_did_change ~notify_back doc c
            ~old_content:(Lsp.Text_document.text old_doc)
            ~new_content:new_st.content

        | Lsp.Client_notification.Exit -> _quit <- true; IO.return ()
        | Lsp.Client_notification.DidSaveTextDocument _
        | Lsp.Client_notification.WillSaveTextDocument _
        | Lsp.Client_notification.ChangeWorkspaceFolders _
        | Lsp.Client_notification.ChangeConfiguration _
        | Lsp.Client_notification.Initialized
        | Lsp.Client_notification.Unknown_notification _
        | Lsp.Client_notification.CancelRequest _
        | Lsp.Client_notification.WorkDoneProgressCancel _
        | Lsp.Client_notification.SetTrace _ (* TODO: a method to setup debug level? *)
          ->
          let notify_back = new notify_back ~notify_back () in
          self#on_notification_unhandled ~notify_back n
      end
  end
end
