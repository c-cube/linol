(** Server interface *)

open Common_
open Sigs

type nonrec doc_state = {
  uri: Lsp.Types.DocumentUri.t;
  languageId: string;
  version: int;
  content: string;
}
(** Current state of a document. *)

(** Request ID.

    The unique ID of a request, used by JSONRPC to map each request to its
    reply. *)
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
            ('a, string) result IO.t
      (** Method called to handle client requests.
          @param notify_back
            an object used to reply to the client, send progress messages,
            diagnostics, etc.
          @param id
            the query RPC ID, can be used for tracing, cancellation, etc. *)

      method must_quit = false
      (** Set to true if the client requested to exit *)

      method virtual spawn_query_handler : (unit -> unit IO.t) -> unit
      (** How to start a new future/task/thread concurrently. This is used to
          process incoming user queries.
          @since 0.5 *)
    end

  let async (self : #base_server) f : unit IO.t =
    self#spawn_query_handler (fun () ->
        IO.catch f (fun exn bt ->
            let msg =
              spf "LSP async notification handler failed with %s\n%s"
                (Printexc.to_string exn)
                (Printexc.raw_backtrace_to_string bt)
            in
            IO.return @@ Log.err (fun k -> k "%s" msg)));
    IO.return ()

  (** A wrapper to more easily reply to notifications *)
  class notify_back ~notify_back ~server_request ~workDoneToken
    ~partialResultToken:_ ?version ?(uri : DocumentUri.t option) () =
    object
      val mutable uri = uri
      method set_uri u = uri <- Some u
      method get_uri = uri

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

      method work_done_progress_begin (p : Lsp.Types.WorkDoneProgressBegin.t) :
          unit IO.t =
        match workDoneToken with
        | Some token ->
          notify_back @@ WorkDoneProgress { token; value = Begin p }
        | None -> IO.return ()

      method work_done_progress_report (p : Lsp.Types.WorkDoneProgressReport.t)
          : unit IO.t =
        match workDoneToken with
        | Some token ->
          notify_back @@ WorkDoneProgress { value = Report p; token }
        | None -> IO.return ()

      method work_done_progress_end (p : Lsp.Types.WorkDoneProgressEnd.t) :
          unit IO.t =
        match workDoneToken with
        | Some token -> notify_back @@ WorkDoneProgress { value = End p; token }
        | None -> IO.return ()

      method send_notification (n : Lsp.Server_notification.t) : unit IO.t =
        notify_back n
      (** Send a notification from the server to the client (general purpose
          method) *)

      method send_request :
          'from_server.
          'from_server Lsp.Server_request.t ->
          (('from_server, Jsonrpc.Response.Error.t) result -> unit IO.t) ->
          Req_id.t IO.t =
        fun r h -> server_request @@ Request_and_handler (r, h)
      (** Send a request from the server to the client (general purpose method)
      *)
    end

  type nonrec doc_state = doc_state = {
    uri: DocumentUri.t;
    languageId: string;
    version: int;
    content: string;
  }
  (** Current state of a document. *)

  let[@inline] lift_ok x =
    let open IO in
    let+ x = x in
    Ok x

  (** An easily overloadable class. Pick the methods you want to support. The
      user must provide at least the callbacks for document lifecycle: open,
      close, update. The most basic LSP server should check documents when
      they're updated and report diagnostics back to the editor. *)
  class virtual server =
    object (self)
      inherit base_server

      val mutable status : [ `Running | `ReceivedShutdown | `ReceivedExit ] =
        `Running

      val mutable positionEncoding : [ `UTF8 | `UTF16 ] = `UTF16
      val docs : (DocumentUri.t, doc_state) Hashtbl.t = Hashtbl.create 16

      method get_status = status
      (** Check if exit or shutdown request was made by the client.
          @since 0.5 *)

      method find_doc (uri : DocumentUri.t) : doc_state option =
        try Some (Hashtbl.find docs uri) with Not_found -> None
      (** Find current state of the given document, if present. *)

      method set_positionEncoding (i : InitializeParams.t) : unit =
        match i.capabilities.general with
        | Some { positionEncodings = Some el; _ } ->
          let l =
            List.filter_map
              (function
                | PositionEncodingKind.UTF8 -> Some `UTF8
                | UTF16 -> Some `UTF16
                | _ -> None)
              el
            |> List.sort_uniq compare
          in
          let encoding =
            match l with
            | [ `UTF8 ] -> `UTF8
            | [] | [ `UTF16 ] | [ `UTF16; _ ] | [ _; `UTF16 ] | _ -> `UTF16
          in
          positionEncoding <- encoding
        | _ -> ()

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

      method config_definition :
          [ `Bool of bool | `DefinitionOptions of DefinitionOptions.t ] option =
        None
      (** @since 0.3 *)

      method config_hover :
          [ `Bool of bool | `HoverOptions of HoverOptions.t ] option =
        None
      (** @since 0.3 *)

      method config_inlay_hints :
          [ `Bool of bool
          | `InlayHintOptions of InlayHintOptions.t
          | `InlayHintRegistrationOptions of InlayHintRegistrationOptions.t
          ]
          option =
        None
      (** Configuration for the inlay hints API. *)

      method config_symbol :
          [ `Bool of bool | `DocumentSymbolOptions of DocumentSymbolOptions.t ]
          option =
        None
      (** @since 0.3 *)

      method config_code_action_provider :
          [ `CodeActionOptions of CodeActionOptions.t | `Bool of bool ] =
        `Bool false
      (** @since 0.3 *)

      method config_modify_capabilities (c : ServerCapabilities.t) :
          ServerCapabilities.t =
        c
      (** Modify capabilities before sending them back to the client. By default
          we just return them unmodified.
          @since 0.3 *)

      method config_list_commands : string list = []
      (** List of commands available *)

      method on_req_initialize ~notify_back:(_ : notify_back)
          (i : InitializeParams.t) : InitializeResult.t IO.t =
        let sync_opts = self#config_sync_opts in
        self#set_positionEncoding i;
        let positionEncoding =
          match positionEncoding with
          | `UTF8 -> PositionEncodingKind.UTF8
          | `UTF16 -> UTF16
        in
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
            ?inlayHintProvider:self#config_inlay_hints
            ?documentSymbolProvider:self#config_symbol
            ~textDocumentSync:(`TextDocumentSyncOptions sync_opts)
            ~positionEncoding ()
          |> self#config_modify_capabilities
        in
        IO.return @@ InitializeResult.create ~capabilities ()

      method on_req_hover ~notify_back:(_ : notify_back) ~id:_ ~uri:_ ~pos:_
          ~workDoneToken:_ (_ : doc_state) : Hover.t option IO.t =
        IO.return None
      (** Called when the user hovers on some identifier in the document *)

      method on_req_completion ~notify_back:(_ : notify_back) ~id:_ ~uri:_
          ~pos:_ ~ctx:_ ~workDoneToken:_ ~partialResultToken:_ (_ : doc_state) :
          [ `CompletionList of CompletionList.t
          | `List of CompletionItem.t list
          ]
          option
          IO.t =
        IO.return None
      (** Called when the user requests completion in the document *)

      method on_req_definition ~notify_back:(_ : notify_back) ~id:_ ~uri:_
          ~pos:_ ~workDoneToken:_ ~partialResultToken:_ (_ : doc_state) :
          Locations.t option IO.t =
        IO.return None
      (** Called when the user wants to jump-to-definition *)

      method on_req_code_lens ~notify_back:(_ : notify_back) ~id:_ ~uri:_
          ~workDoneToken:_ ~partialResultToken:_ (_ : doc_state) :
          CodeLens.t list IO.t =
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
          ~workDoneToken:_ (_c : string) (_args : Yojson.Safe.t list option) :
          Yojson.Safe.t IO.t =
        IO.return `Null
      (** Execute a command with given arguments.
          @since 0.3 *)

      method on_req_symbol ~notify_back:(_ : notify_back) ~id:_ ~uri:_
          ~workDoneToken:_ ~partialResultToken:_ () :
          [ `DocumentSymbol of DocumentSymbol.t list
          | `SymbolInformation of SymbolInformation.t list
          ]
          option
          IO.t =
        IO.return None
      (** List symbols in this document.
          @since 0.3 *)

      method on_unknown_request ~notify_back:(_ : notify_back) ~server_request:_
          ~id:_ _meth _params : Yojson.Safe.t IO.t =
        IO.failwith "unhandled request"

      method on_req_inlay_hint ~notify_back:(_ : notify_back) ~id:_ ~uri:_
          ~range:(_ : Lsp.Types.Range.t) () :
          Lsp.Types.InlayHint.t list option IO.t =
        IO.return None
      (** Provide inlay hints for this document.
          @since 0.5 *)

      method on_req_shutdown ~notify_back:(_ : notify_back) ~id:_ : unit IO.t =
        IO.return ()
      (** Process a shutdown request.
          @since 0.7 *)

      method on_request : type r.
          notify_back:_ ->
          server_request:_ ->
          id:Req_id.t ->
          r Lsp.Client_request.t ->
          (r, string) result IO.t =
        fun ~notify_back ~server_request ~id (r : _ Lsp.Client_request.t) ->
          Trace.with_span ~__FILE__ ~__LINE__ "linol.on-request"
          @@ fun _sp : (r, string) result IO.t ->
          (* handler to catch all errors *)
          let try_catch : (unit -> (r, _) result IO.t) -> (r, _) result IO.t =
           fun f ->
            IO.catch f (fun exn bt ->
                let msg =
                  spf "LSP request handler failed with %s\n%s"
                    (Printexc.to_string exn)
                    (Printexc.raw_backtrace_to_string bt)
                in
                Log.err (fun k -> k "%s" msg);
                IO.return @@ Error msg)
          in

          try_catch @@ fun () ->
          Log.debug (fun k ->
              k "handle request[id=%s] <opaque>" (Req_id.to_string id));

          match r with
          | Lsp.Client_request.Shutdown ->
            Log.info (fun k -> k "shutdown");
            let notify_back =
              new notify_back
                ~workDoneToken:None ~partialResultToken:None ~notify_back
                ~server_request ()
            in
            status <- `ReceivedShutdown;
            lift_ok @@ self#on_req_shutdown ~notify_back ~id
          | Lsp.Client_request.Initialize i ->
            Log.debug (fun k -> k "req: initialize");
            let notify_back =
              new notify_back
                ~partialResultToken:None ~workDoneToken:i.workDoneToken
                ~notify_back ~server_request ()
            in
            self#set_positionEncoding i;
            lift_ok @@ self#on_req_initialize ~notify_back i
          | Lsp.Client_request.TextDocumentHover
              { textDocument; position; workDoneToken } ->
            let uri = textDocument.uri in
            Log.debug (fun k -> k "req: hover '%s'" (DocumentUri.to_path uri));

            (match Hashtbl.find_opt docs uri with
            | None -> IO.return @@ Ok None
            | Some doc_st ->
              let notify_back =
                new notify_back
                  ~workDoneToken ~partialResultToken:None ~uri ~notify_back
                  ~server_request ()
              in
              lift_ok
              @@ self#on_req_hover ~notify_back ~id ~uri ~pos:position
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
            | None -> IO.return @@ Ok None
            | Some doc_st ->
              let notify_back =
                new notify_back
                  ~partialResultToken ~workDoneToken ~uri ~notify_back
                  ~server_request ()
              in
              lift_ok
              @@ self#on_req_completion ~notify_back ~id ~uri ~workDoneToken
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
            | None -> IO.return @@ Ok None
            | Some doc_st ->
              lift_ok
              @@ self#on_req_definition ~notify_back ~id ~workDoneToken
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
            | None -> IO.return @@ Ok []
            | Some doc_st ->
              lift_ok
              @@ self#on_req_code_lens ~notify_back ~id ~uri ~workDoneToken
                   ~partialResultToken doc_st)
          | Lsp.Client_request.TextDocumentCodeLensResolve cl ->
            Log.debug (fun k -> k "req: codelens resolve");
            let notify_back =
              new notify_back
                ~workDoneToken:None ~partialResultToken:None ~notify_back
                ~server_request ()
            in
            lift_ok @@ self#on_req_code_lens_resolve ~notify_back ~id cl
          | Lsp.Client_request.ExecuteCommand
              { command; arguments; workDoneToken } ->
            Log.debug (fun k -> k "req: execute command '%s'" command);
            let notify_back =
              new notify_back
                ~workDoneToken ~partialResultToken:None ~notify_back
                ~server_request ()
            in
            lift_ok
            @@ self#on_req_execute_command ~notify_back ~id ~workDoneToken
                 command arguments
          | Lsp.Client_request.DocumentSymbol
              { textDocument = d; workDoneToken; partialResultToken } ->
            let notify_back =
              new notify_back
                ~workDoneToken ~partialResultToken ~notify_back ~server_request
                ()
            in
            lift_ok
            @@ self#on_req_symbol ~notify_back ~id ~uri:d.uri ~workDoneToken
                 ~partialResultToken ()
          | Lsp.Client_request.CodeAction a ->
            let notify_back =
              new notify_back
                ~workDoneToken:a.workDoneToken
                ~partialResultToken:a.partialResultToken ~notify_back
                ~server_request ()
            in
            lift_ok @@ self#on_req_code_action ~notify_back ~id a
          | Lsp.Client_request.InlayHint p ->
            let notify_back : notify_back =
              new notify_back
                ~workDoneToken:p.workDoneToken ~partialResultToken:None
                ~notify_back ~server_request ()
            in
            lift_ok
            @@ self#on_req_inlay_hint ~notify_back ~id ~uri:p.textDocument.uri
                 ~range:p.range ()
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
          | Lsp.Client_request.InlayHintResolve _
          | Lsp.Client_request.TextDocumentDiagnostic _
          | Lsp.Client_request.TextDocumentInlineCompletion _
          | Lsp.Client_request.TextDocumentInlineValue _
          | Lsp.Client_request.TextDocumentPrepareTypeHierarchy _
          | Lsp.Client_request.TextDocumentRangesFormatting _
          | Lsp.Client_request.WorkspaceSymbolResolve _
          | Lsp.Client_request.WorkspaceDiagnostic _
          | Lsp.Client_request.TypeHierarchySubtypes _
          | Lsp.Client_request.TypeHierarchySupertypes _ ->
            let notify_back =
              new notify_back
                ~workDoneToken:None ~partialResultToken:None ~notify_back
                ~server_request ()
            in
            lift_ok @@ self#on_request_unhandled ~notify_back ~id r
          | Lsp.Client_request.UnknownRequest r ->
            let notify_back =
              new notify_back
                ~workDoneToken:None ~partialResultToken:None ~notify_back
                ~server_request ()
            in
            lift_ok
            @@ self#on_unknown_request ~notify_back ~server_request ~id r.meth
                 r.params

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

      method on_notif_doc_did_save ~notify_back:(_ : notify_back)
          (_params : DidSaveTextDocumentParams.t) : unit IO.t =
        IO.return ()

      method on_unknown_notification ~notify_back:(_ : notify_back)
          (_n : Jsonrpc.Notification.t) : unit IO.t =
        IO.return ()

      method on_notification_unhandled ~notify_back:(_ : notify_back)
          (_n : Lsp.Client_notification.t) : unit IO.t =
        IO.return ()
      (** Override to handle unprocessed notifications *)

      method filter_text_document (_doc_uri : Lsp.Types.DocumentUri.t) : bool =
        true
      (** Filter the document URI to check if we want to process it or not. By
          default we accept all documents. *)

      method on_notification ~notify_back ~server_request
          (n : Lsp.Client_notification.t) : unit IO.t =
        let@ _sp =
          Trace.with_span ~__FILE__ ~__LINE__ "linol.on-notification"
        in

        (* handler to catch all errors *)
        let try_catch : (unit -> unit IO.t) -> unit IO.t =
         fun f ->
          IO.catch f (fun exn bt ->
              let msg =
                spf "LSP notification handler failed with %s\n%s"
                  (Printexc.to_string exn)
                  (Printexc.raw_backtrace_to_string bt)
              in
              Log.err (fun k -> k "%s" msg);
              IO.return ())
        in

        try_catch @@ fun () ->
        let open Lsp.Types in
        match n with
        | Lsp.Client_notification.TextDocumentDidOpen
            { DidOpenTextDocumentParams.textDocument = doc } ->
          if not (self#filter_text_document doc.uri) then
            IO.return ()
          else (
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

            async self (fun () ->
                self#on_notif_doc_did_open
                  ~notify_back:(notify_back : notify_back)
                  doc ~content:st.content)
          )
        | Lsp.Client_notification.TextDocumentDidClose { textDocument = doc } ->
          if not (self#filter_text_document doc.uri) then
            IO.return ()
          else (
            Log.debug (fun k ->
                k "notif: did close '%s'" (DocumentUri.to_path doc.uri));
            let notify_back =
              new notify_back
                ~workDoneToken:None ~partialResultToken:None ~uri:doc.uri
                ~notify_back ~server_request ()
            in

            async self (fun () ->
                self#on_notif_doc_did_close
                  ~notify_back:(notify_back : notify_back)
                  doc)
          )
        | Lsp.Client_notification.TextDocumentDidChange
            { textDocument = doc; contentChanges = c } ->
          if not (self#filter_text_document doc.uri) then
            IO.return ()
          else (
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
                Lsp.Text_document.make ~position_encoding:positionEncoding
                  (DidOpenTextDocumentParams.create
                     ~textDocument:
                       (TextDocumentItem.create ~languageId ~uri:doc.uri
                          ~version ~text:""))
              | Some st ->
                Lsp.Text_document.make ~position_encoding:positionEncoding
                  (DidOpenTextDocumentParams.create
                     ~textDocument:
                       (TextDocumentItem.create ~languageId:st.languageId
                          ~uri:doc.uri ~version:st.version ~text:st.content))
            in

            let new_doc : Lsp.Text_document.t =
              Lsp.Text_document.apply_content_changes old_doc c
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

            async self (fun () ->
                self#on_notif_doc_did_change
                  ~notify_back:(notify_back : notify_back)
                  doc c
                  ~old_content:(Lsp.Text_document.text old_doc)
                  ~new_content:new_st.content)
          )
        | Lsp.Client_notification.DidSaveTextDocument params ->
          if not (self#filter_text_document params.textDocument.uri) then
            IO.return ()
          else (
            let notify_back =
              new notify_back
                ~workDoneToken:None ~partialResultToken:None
                ~uri:params.textDocument.uri ~notify_back ~server_request ()
            in

            async self (fun () ->
                self#on_notif_doc_did_save
                  ~notify_back:(notify_back : notify_back)
                  params)
          )
        | Lsp.Client_notification.Exit ->
          status <- `ReceivedExit;
          IO.return ()
        | Lsp.Client_notification.WillSaveTextDocument _
        | Lsp.Client_notification.ChangeWorkspaceFolders _
        | Lsp.Client_notification.ChangeConfiguration _
        | Lsp.Client_notification.Initialized
        | Lsp.Client_notification.CancelRequest _
        | Lsp.Client_notification.WorkDoneProgressCancel _
        | Lsp.Client_notification.SetTrace _
        | Lsp.Client_notification.DidChangeWatchedFiles _
        | Lsp.Client_notification.DidCreateFiles _
        | Lsp.Client_notification.DidDeleteFiles _
        | Lsp.Client_notification.WorkDoneProgress _
        | Lsp.Client_notification.DidRenameFiles _
        | Lsp.Client_notification.NotebookDocumentDidOpen _
        | Lsp.Client_notification.NotebookDocumentDidChange _
        | Lsp.Client_notification.NotebookDocumentDidClose _
        | Lsp.Client_notification.NotebookDocumentDidSave _ ->
          let notify_back =
            new notify_back
              ~workDoneToken:None ~partialResultToken:None ~notify_back
              ~server_request ()
          in

          async self (fun () ->
              self#on_notification_unhandled
                ~notify_back:(notify_back : notify_back)
                n)
        | Lsp.Client_notification.UnknownNotification n ->
          let notify_back =
            new notify_back
              ~workDoneToken:None ~partialResultToken:None ~notify_back
              ~server_request ()
          in

          async self (fun () -> self#on_unknown_notification ~notify_back n)
    end
end
