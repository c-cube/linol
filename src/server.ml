(** Server interface *)

open Sigs

type nonrec doc_state = {
  uri: Lsp.Types.DocumentUri.t;
  languageId: string;
  version: int;
  content: string;
}
(** Current state of a document. *)

(** Request ID.

    The unique ID of a request, used by JSONRPC to map each request to its reply. *)
module Req_id = struct
  type t = Jsonrpc.Id.t

  (** String representation of the ID *)
  let to_string : t -> string = function
    | `String s -> s
    | `Int i -> string_of_int i
end

(** Server interface for some IO substrate. *)
module Make (IO : IO) = struct
  open Lsp.Types
  module Position = Position
  module Range = Range
  module Diagnostic = Diagnostic
  module DiagnosticSeverity = DiagnosticSeverity
  module Req_id = Req_id

  (** A variant carrying a [Lsp.Server_request.t] and a handler for its return
      value. The request is stored in order to allow us to discriminate its
      existential variable. *)
  type server_request_handler_pair =
    | Request_and_handler :
        'from_server Lsp.Server_request.t
        * (('from_server, Jsonrpc.Response.Error.t) result -> unit IO.t)
        -> server_request_handler_pair

  type send_request = server_request_handler_pair -> Req_id.t IO.t
  (** The type of the action that sends a request from the server to the client
      and handles its response. *)

  (** The server baseclass *)
  class virtual base_server =
    object
      method virtual on_notification
          : notify_back:(Lsp.Server_notification.t -> unit IO.t) ->
            server_request:send_request ->
            Lsp.Client_notification.t ->
            unit IO.t

      method virtual on_request
          : 'a.
            notify_back:(Lsp.Server_notification.t -> unit IO.t) ->
            server_request:send_request ->
            id:Req_id.t ->
            'a Lsp.Client_request.t ->
            'a IO.t
      (** Method called to handle client requests.
        @param notify_back an object used to reply to the client, send progress
        messages, diagnostics, etc.
        @param id the query RPC ID, can be used for tracing, cancellation, etc. *)

      method must_quit = false
      (** Set to true if the client requested to exit *)

      method virtual spawn_query_handler : (unit -> unit IO.t) -> unit
      (** How to start a new future/task/thread concurrently. This is used
          to process incoming user queries.
          @since NEXT_RELEASE *)
    end

  (** A wrapper to more easily reply to notifications *)
  class notify_back ~notify_back ~server_request ~workDoneToken
    ~partialResultToken:_ ?version ?(uri : DocumentUri.t option) () =
    object
      val mutable uri = uri
      method set_uri u = uri <- Some u

      method send_log_msg ~type_ msg : unit IO.t =
        let params = LogMessageParams.create ~type_ ~message:msg in
        notify_back (Lsp.Server_notification.LogMessage params)
      (** Send a log message to the editor *)

      method send_diagnostic (l : Diagnostic.t list) : unit IO.t =
        match uri with
        | None ->
          IO.failwith "notify_back: cannot publish diagnostics, no URI given"
        | Some uri ->
          let params =
            PublishDiagnosticsParams.create ~uri ?version ~diagnostics:l ()
          in
          notify_back (Lsp.Server_notification.PublishDiagnostics params)
      (** Send diagnostics for the current document *)

      method telemetry json : unit IO.t =
        notify_back @@ Lsp.Server_notification.TelemetryNotification json

      method cancel_request (id : Jsonrpc.Id.t) : unit IO.t =
        notify_back @@ CancelRequest id

      method work_done_progress_begin (p : Lsp.Types.WorkDoneProgressBegin.t)
          : unit IO.t =
        match workDoneToken with
        | Some token ->
          notify_back
          @@ WorkDoneProgress
               { value = Lsp.Server_notification.Progress.Begin p; token }
        | None -> IO.return ()

      method work_done_progress_report (p : Lsp.Types.WorkDoneProgressReport.t)
          : unit IO.t =
        match workDoneToken with
        | Some token ->
          notify_back
          @@ WorkDoneProgress
               { value = Lsp.Server_notification.Progress.Report p; token }
        | None -> IO.return ()

      method work_done_progress_end (p : Lsp.Types.WorkDoneProgressEnd.t)
          : unit IO.t =
        match workDoneToken with
        | Some token ->
          notify_back
          @@ WorkDoneProgress
               { value = Lsp.Server_notification.Progress.End p; token }
        | None -> IO.return ()

      method send_notification (n : Lsp.Server_notification.t) = notify_back n
      (** Send a notification from the server to the client (general purpose method) *)

      method send_request
          : 'from_server.
            'from_server Lsp.Server_request.t ->
            (('from_server, Jsonrpc.Response.Error.t) result -> unit IO.t) ->
            Req_id.t IO.t =
        fun r h -> server_request @@ Request_and_handler (r, h)
      (** Send a request from the server to the client (general purpose method) *)
    end

  type nonrec doc_state = doc_state = {
    uri: DocumentUri.t;
    languageId: string;
    version: int;
    content: string;
  }
  (** Current state of a document. *)

  (** An easily overloadable class. Pick the methods you want to support.
      The user must provide at least the callbacks for document lifecycle:
      open, close, update. The most basic LSP server should check documents
      when they're updated and report diagnostics back to the editor. *)
  class virtual server =
    object (self)
      inherit base_server

      val mutable status : [ `Running | `ReceivedShutdown | `ReceivedExit ] =
        `Running

      val docs : (DocumentUri.t, doc_state) Hashtbl.t = Hashtbl.create 16
      method get_status = status
      (** Check if exit or shutdown request was made by the client.
        @since NEXT_RELEASE *)

      method find_doc (uri : DocumentUri.t) : doc_state option =
        try Some (Hashtbl.find docs uri) with Not_found -> None
      (** Find current state of the given document, if present. *)

      method on_request_unhandled : type r.
          notify_back:notify_back ->
          id:Req_id.t ->
          r Lsp.Client_request.t ->
          r IO.t =
        fun ~notify_back:(_ : notify_back) ~id:_ _r ->
          Log.debug (fun k -> k "req: unhandled request");
          IO.failwith "TODO: handle this request"
      (** Override to process other requests *)

      method config_sync_opts : TextDocumentSyncOptions.t =
        TextDocumentSyncOptions.create ~change:TextDocumentSyncKind.Incremental
          ~openClose:true
          ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
          ~willSave:false ()
      (** Parameter for how to synchronize content with the editor *)

      method config_completion : CompletionOptions.t option = None
      (** Configuration for the completion API.
        @since 0.4 *)

      method config_code_lens_options : CodeLensOptions.t option = None
      (** @since 0.3 *)

      method config_definition
          : [ `Bool of bool | `DefinitionOptions of DefinitionOptions.t ] option
          =
        None
      (** @since 0.3 *)

      method config_hover
          : [ `Bool of bool | `HoverOptions of HoverOptions.t ] option =
        None
      (** @since 0.3 *)

      method config_symbol
          : [ `Bool of bool
            | `DocumentSymbolOptions of DocumentSymbolOptions.t
            ]
            option =
        None
      (** @since 0.3 *)

      method config_code_action_provider
          : [ `CodeActionOptions of CodeActionOptions.t | `Bool of bool ] =
        `Bool false
      (** @since 0.3 *)

      method config_modify_capabilities (c : ServerCapabilities.t)
          : ServerCapabilities.t =
        c
      (** Modify capabilities before sending them back to the client.
        By default we just return them unmodified.
        @since 0.3 *)

      method config_list_commands : string list = []
      (** List of commands available *)

      method on_req_initialize ~notify_back:(_ : notify_back)
          (_i : InitializeParams.t) : InitializeResult.t IO.t =
        let sync_opts = self#config_sync_opts in
        let capabilities =
          ServerCapabilities.create
            ?codeLensProvider:self#config_code_lens_options
            ~codeActionProvider:self#config_code_action_provider
            ~executeCommandProvider:
              (ExecuteCommandOptions.create ~commands:self#config_list_commands
                 ())
            ?completionProvider:self#config_completion
            ?definitionProvider:self#config_definition
            ?hoverProvider:self#config_hover
            ?documentSymbolProvider:self#config_symbol
            ~textDocumentSync:(`TextDocumentSyncOptions sync_opts) ()
          |> self#config_modify_capabilities
        in
        IO.return @@ InitializeResult.create ~capabilities ()

      method on_req_hover ~notify_back:(_ : notify_back) ~id:_ ~uri:_ ~pos:_
          ~workDoneToken:_ (_ : doc_state) : Hover.t option IO.t =
        IO.return None
      (** Called when the user hovers on some identifier in the document *)

      method on_req_completion ~notify_back:(_ : notify_back) ~id:_ ~uri:_
          ~pos:_ ~ctx:_ ~workDoneToken:_ ~partialResultToken:_ (_ : doc_state)
          : [ `CompletionList of CompletionList.t
            | `List of CompletionItem.t list
            ]
            option
            IO.t =
        IO.return None
      (** Called when the user requests completion in the document *)

      method on_req_definition ~notify_back:(_ : notify_back) ~id:_ ~uri:_
          ~pos:_ ~workDoneToken:_ ~partialResultToken:_ (_ : doc_state)
          : Locations.t option IO.t =
        IO.return None
      (** Called when the user wants to jump-to-definition  *)

      method on_req_code_lens ~notify_back:(_ : notify_back) ~id:_ ~uri:_
          ~workDoneToken:_ ~partialResultToken:_ (_ : doc_state)
          : CodeLens.t list IO.t =
        IO.return []
      (** List code lenses for the given document
        @since 0.3 *)

      method on_req_code_lens_resolve ~notify_back:(_ : notify_back) ~id:_
          (cl : CodeLens.t) : CodeLens.t IO.t =
        IO.return cl
      (** Code lens resolution, must return a code lens with non null "command"
        @since 0.3 *)

      method on_req_code_action ~notify_back:(_ : notify_back) ~id:_
          (_c : CodeActionParams.t) : CodeActionResult.t IO.t =
        IO.return None
      (** Code action.
        @since 0.3 *)

      method on_req_execute_command ~notify_back:(_ : notify_back) ~id:_
          ~workDoneToken:_ (_c : string) (_args : Yojson.Safe.t list option)
          : Yojson.Safe.t IO.t =
        IO.return `Null
      (** Execute a command with given arguments.
        @since 0.3 *)

      method on_req_symbol ~notify_back:(_ : notify_back) ~id:_ ~uri:_
          ~workDoneToken:_ ~partialResultToken:_ ()
          : [ `DocumentSymbol of DocumentSymbol.t list
            | `SymbolInformation of SymbolInformation.t list
            ]
            option
            IO.t =
        IO.return None
      (** List symbols in this document.
        @since 0.3 *)

      method on_request : type r.
          notify_back:_ ->
          server_request:_ ->
          id:Req_id.t ->
          r Lsp.Client_request.t ->
          r IO.t =
        fun ~notify_back ~server_request ~id (r : _ Lsp.Client_request.t) ->
          Log.debug (fun k ->
              k "handle request[id=%s] <opaque>" (Req_id.to_string id));

          match r with
          | Lsp.Client_request.Shutdown ->
            Log.info (fun k -> k "shutdown");
            status <- `ReceivedShutdown;
            IO.return ()
          | Lsp.Client_request.Initialize i ->
            Log.debug (fun k -> k "req: initialize");
            let notify_back =
              new notify_back
                ~partialResultToken:None ~workDoneToken:i.workDoneToken
                ~notify_back ~server_request ()
            in
            self#on_req_initialize ~notify_back i
          | Lsp.Client_request.TextDocumentHover
              { textDocument; position; workDoneToken } ->
            let uri = textDocument.uri in
            Log.debug (fun k -> k "req: hover '%s'" (DocumentUri.to_path uri));

            (match Hashtbl.find_opt docs uri with
            | None -> IO.return None
            | Some doc_st ->
              let notify_back =
                new notify_back
                  ~workDoneToken ~partialResultToken:None ~uri ~notify_back
                  ~server_request ()
              in
              self#on_req_hover ~notify_back ~id ~uri ~pos:position
                ~workDoneToken doc_st)
          | Lsp.Client_request.TextDocumentCompletion
              {
                textDocument;
                position;
                context;
                workDoneToken;
                partialResultToken;
              } ->
            let uri = textDocument.uri in
            Log.debug (fun k ->
                k "req: complete '%s'" (DocumentUri.to_path uri));
            (match Hashtbl.find_opt docs uri with
            | None -> IO.return None
            | Some doc_st ->
              let notify_back =
                new notify_back
                  ~partialResultToken ~workDoneToken ~uri ~notify_back
                  ~server_request ()
              in
              self#on_req_completion ~notify_back ~id ~uri ~workDoneToken
                ~partialResultToken ~pos:position ~ctx:context doc_st)
          | Lsp.Client_request.TextDocumentDefinition
              { textDocument; position; workDoneToken; partialResultToken } ->
            let uri = textDocument.uri in
            Log.debug (fun k ->
                k "req: definition '%s'" (DocumentUri.to_path uri));
            let notify_back =
              new notify_back
                ~workDoneToken ~partialResultToken ~uri ~notify_back
                ~server_request ()
            in

            (match Hashtbl.find_opt docs uri with
            | None -> IO.return None
            | Some doc_st ->
              self#on_req_definition ~notify_back ~id ~workDoneToken
                ~partialResultToken ~uri ~pos:position doc_st)
          | Lsp.Client_request.TextDocumentCodeLens
              { textDocument; workDoneToken; partialResultToken } ->
            let uri = textDocument.uri in
            Log.debug (fun k ->
                k "req: codelens '%s'" (DocumentUri.to_path uri));
            let notify_back =
              new notify_back
                ~workDoneToken ~partialResultToken ~uri ~notify_back
                ~server_request ()
            in

            (match Hashtbl.find_opt docs uri with
            | None -> IO.return []
            | Some doc_st ->
              self#on_req_code_lens ~notify_back ~id ~uri ~workDoneToken
                ~partialResultToken doc_st)
          | Lsp.Client_request.TextDocumentCodeLensResolve cl ->
            Log.debug (fun k -> k "req: codelens resolve");
            let notify_back =
              new notify_back
                ~workDoneToken:None ~partialResultToken:None ~notify_back
                ~server_request ()
            in
            self#on_req_code_lens_resolve ~notify_back ~id cl
          | Lsp.Client_request.ExecuteCommand
              { command; arguments; workDoneToken } ->
            Log.debug (fun k -> k "req: execute command '%s'" command);
            let notify_back =
              new notify_back
                ~workDoneToken ~partialResultToken:None ~notify_back
                ~server_request ()
            in
            self#on_req_execute_command ~notify_back ~id ~workDoneToken command
              arguments
          | Lsp.Client_request.DocumentSymbol
              { textDocument = d; workDoneToken; partialResultToken } ->
            let notify_back =
              new notify_back
                ~workDoneToken ~partialResultToken ~notify_back ~server_request
                ()
            in
            self#on_req_symbol ~notify_back ~id ~uri:d.uri ~workDoneToken
              ~partialResultToken ()
          | Lsp.Client_request.CodeAction a ->
            let notify_back =
              new notify_back
                ~workDoneToken:a.workDoneToken
                ~partialResultToken:a.partialResultToken ~notify_back
                ~server_request ()
            in
            self#on_req_code_action ~notify_back ~id a
          | Lsp.Client_request.CodeActionResolve _
          | Lsp.Client_request.LinkedEditingRange _
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
          | Lsp.Client_request.TextDocumentMoniker _
          | Lsp.Client_request.TextDocumentOnTypeFormatting _
          | Lsp.Client_request.TextDocumentColorPresentation _
          | Lsp.Client_request.TextDocumentColor _
          | Lsp.Client_request.SelectionRange _
          | Lsp.Client_request.SemanticTokensDelta _
          | Lsp.Client_request.SemanticTokensFull _
          | Lsp.Client_request.SemanticTokensRange _
          | Lsp.Client_request.TextDocumentImplementation _
          | Lsp.Client_request.TextDocumentPrepareCallHierarchy _
          | Lsp.Client_request.TextDocumentRangeFormatting _
          | Lsp.Client_request.CallHierarchyIncomingCalls _
          | Lsp.Client_request.CallHierarchyOutgoingCalls _
          | Lsp.Client_request.WillCreateFiles _
          | Lsp.Client_request.WillDeleteFiles _
          | Lsp.Client_request.WillRenameFiles _
          | Lsp.Client_request.UnknownRequest _ ->
            let notify_back =
              new notify_back
                ~workDoneToken:None ~partialResultToken:None ~notify_back
                ~server_request ()
            in
            self#on_request_unhandled ~notify_back ~id r

      method virtual on_notif_doc_did_open
          : notify_back:notify_back ->
            TextDocumentItem.t ->
            content:string ->
            unit IO.t
      (** Called when a document is opened *)

      method virtual on_notif_doc_did_close
          : notify_back:notify_back -> TextDocumentIdentifier.t -> unit IO.t

      method virtual on_notif_doc_did_change
          : notify_back:notify_back ->
            VersionedTextDocumentIdentifier.t ->
            TextDocumentContentChangeEvent.t list ->
            old_content:string ->
            new_content:string ->
            unit IO.t
      (** Called when the document changes. *)

      method on_notification_unhandled ~notify_back:(_ : notify_back)
          (_n : Lsp.Client_notification.t) : unit IO.t =
        IO.return ()
      (** Override to handle unprocessed notifications *)

      method on_notification ~notify_back ~server_request
          (n : Lsp.Client_notification.t) : unit IO.t =
        let open Lsp.Types in
        match n with
        | Lsp.Client_notification.TextDocumentDidOpen
            { DidOpenTextDocumentParams.textDocument = doc } ->
          Log.debug (fun k ->
              k "notif: did open '%s'" (DocumentUri.to_path doc.uri));
          let notify_back =
            new notify_back
              ~uri:doc.uri ~workDoneToken:None ~partialResultToken:None
              ~version:doc.version ~notify_back ~server_request ()
          in
          let st =
            {
              uri = doc.uri;
              version = doc.version;
              content = doc.text;
              languageId = doc.languageId;
            }
          in
          Hashtbl.replace docs doc.uri st;
          self#on_notif_doc_did_open
            ~notify_back:(notify_back : notify_back)
            doc ~content:st.content
        | Lsp.Client_notification.TextDocumentDidClose { textDocument = doc } ->
          Log.debug (fun k ->
              k "notif: did close '%s'" (DocumentUri.to_path doc.uri));
          let notify_back =
            new notify_back
              ~workDoneToken:None ~partialResultToken:None ~uri:doc.uri
              ~notify_back ~server_request ()
          in
          self#on_notif_doc_did_close
            ~notify_back:(notify_back : notify_back)
            doc
        | Lsp.Client_notification.TextDocumentDidChange
            { textDocument = doc; contentChanges = c } ->
          Log.debug (fun k ->
              k "notif: did change '%s'" (DocumentUri.to_path doc.uri));
          let notify_back =
            new notify_back
              ~workDoneToken:None ~partialResultToken:None ~uri:doc.uri
              ~notify_back ~server_request ()
          in

          let old_doc =
            match Hashtbl.find_opt docs doc.uri with
            | None ->
              (* WTF vscode. Well let's try and deal with it. *)
              Log.err (fun k ->
                  k "unknown document: '%s'" (DocumentUri.to_path doc.uri));
              let version = doc.version in

              let languageId = "" in
              (* FIXME*)
              Lsp.Text_document.make
                (DidOpenTextDocumentParams.create
                   ~textDocument:
                     (TextDocumentItem.create ~languageId ~uri:doc.uri ~version
                        ~text:""))
            | Some st ->
              Lsp.Text_document.make
                (DidOpenTextDocumentParams.create
                   ~textDocument:
                     (TextDocumentItem.create ~languageId:st.languageId
                        ~uri:doc.uri ~version:st.version ~text:st.content))
          in

          let new_doc : Lsp.Text_document.t =
            List.fold_left
              (fun d ev -> Lsp.Text_document.apply_content_change d ev)
              old_doc c
          in

          let new_st : doc_state =
            {
              uri = doc.uri;
              languageId = Lsp.Text_document.languageId new_doc;
              content = Lsp.Text_document.text new_doc;
              version = Lsp.Text_document.version new_doc;
            }
          in

          Hashtbl.replace docs doc.uri new_st;
          self#on_notif_doc_did_change
            ~notify_back:(notify_back : notify_back)
            doc c
            ~old_content:(Lsp.Text_document.text old_doc)
            ~new_content:new_st.content
        | Lsp.Client_notification.Exit ->
          status <- `ReceivedExit;
          IO.return ()
        | Lsp.Client_notification.DidSaveTextDocument _
        | Lsp.Client_notification.WillSaveTextDocument _
        | Lsp.Client_notification.ChangeWorkspaceFolders _
        | Lsp.Client_notification.ChangeConfiguration _
        | Lsp.Client_notification.Initialized
        | Lsp.Client_notification.UnknownNotification _
        | Lsp.Client_notification.CancelRequest _
        | Lsp.Client_notification.WorkDoneProgressCancel _
        | Lsp.Client_notification.SetTrace _
        | Lsp.Client_notification.DidChangeWatchedFiles _
        | Lsp.Client_notification.DidCreateFiles _
        | Lsp.Client_notification.DidDeleteFiles _
        | Lsp.Client_notification.DidRenameFiles _
        | Lsp.Client_notification.LogTrace _ ->
          let notify_back =
            new notify_back
              ~workDoneToken:None ~partialResultToken:None ~notify_back
              ~server_request ()
          in
          self#on_notification_unhandled
            ~notify_back:(notify_back : notify_back)
            n
    end
end
