(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Vscode

type handler =
  | Instance of     (* Intended for partial application of [handler instance] *)
      (Superbol_instance.t -> args:Ojs.t list -> unit)
  (* | Text of *)
  (*     (Superbol_instance.t -> textEditor:TextEditor.t -> *)
  (*      edit:TextEditorEdit.t -> args:Ojs.t list -> unit) *)

type t =
  {
    id: string;
    handler: handler;
  }

let extension_oc : Vscode.OutputChannel.t Lazy.t =
  lazy (Vscode.Window.createOutputChannel ~name:"SuperBOL Studio Extension")

let log_line fmt =
  Printf.ksprintf
    (fun value -> OutputChannel.appendLine (Lazy.force extension_oc) ~value)
    fmt

let commands = ref []

let command id handler =
  let command = { id; handler } in
  commands := command :: !commands;
  command

let _open_cfg =
  command "superbol.cfg.open" @@ Instance
    begin fun _instance ~args:_ ->
      let _ : unit Promise.t = Superbol_cfg_explorer.open_cfg
          ~typ:Graphviz _instance in
      ()
    end

let _open_cfg_arc =
  command "superbol.cfg.open.arc" @@ Instance
    begin fun _instance ~args:_ ->
      let _ : unit Promise.t = Superbol_cfg_explorer.open_cfg
          ~typ:D3_arc_diagram _instance in
      ()
    end

let _editor_action_findReferences =
  let command_name = "superbol.editor.action.findReferences"  in
  command command_name @@ Instance
    begin fun _instance ~args ->
      match args with
      | [arg1; arg2] ->
        let uri = Uri.t_to_js @@ Uri.parse (Ojs.string_of_js arg1) () in
        let pos =
          let line = Ojs.get_prop_ascii arg2 "line" |> Ojs.int_of_js in
          let character = Ojs.get_prop_ascii arg2 "character" |> Ojs.int_of_js in
          Position.t_to_js @@ Position.make ~line ~character in
        let _ = Commands.executeCommand
            ~command:"editor.action.findReferences"
            ~args:[uri; pos]
        in ()
      | _ ->
        let types_given = List.map Ojs.type_of args |> String.concat ", " in
        let lazy oc = extension_oc in
        let value = Printf.sprintf
            "Internal warning: unexpected arguments given to %s: \
             expected uri & position, got [%s]" command_name types_given in
        OutputChannel.appendLine oc ~value
    end

(** {2 Workspace-wide analysis} *)

(* Diagnostics stay in the Problems view once the document is closed. *)

(* Same extensions as the `cobol' language contribution. *)
let cobol_file_patterns =
  [
    "**/*.[cC]{ob,OB,bl,BL,py,PY,bx,BX,bsql}";
    "**/*.[pP]{co,CO}";
  ]

let find_cobol_files ~token =
  let open Promise.Syntax in
  let rec aux acc = function
    | [] ->
        Promise.return (List.rev acc)
    | pattern :: patterns ->
        let* uris = Workspace.findFiles () ~includes:(`String pattern) ~token in
        aux (List.rev_append uris acc) patterns
  in
  aux [] cobol_file_patterns

(* One analysis at a time, so the status bar button knows what to stop. *)
type run = { mutable stopped: bool }

let current_run: run option ref = ref None

let stop_button =
  lazy (Window.createStatusBarItem ~alignment:StatusBarAlignment.Left
          ~priority:10 ())

(* Drives the `enablement' of the stop command in the palette. *)
let set_analyzing analyzing =
  let _ =
    Commands.executeCommand ~command:"setContext"
      ~args:[Ojs.string_to_js "superbol.analyzing"; Ojs.bool_to_js analyzing]
  in
  ()

let start_run () =
  let run = { stopped = false } in
  current_run := Some run;
  set_analyzing true;
  let lazy button = stop_button in
  StatusBarItem.set_text button "$(debug-stop) Stop analysis";
  StatusBarItem.set_tooltip button "Stop the SuperBOL workspace analysis";
  StatusBarItem.set_command button (`String "superbol.analyze.stop");
  StatusBarItem.show button;
  run

let clear_run () =
  current_run := None;
  set_analyzing false;
  StatusBarItem.hide (Lazy.force stop_button)

(* Only the current run may clear the button: a wedged run can settle long
   after the user gave up on it and started another one. *)
let end_run run =
  match !current_run with
  | Some current when current == run -> clear_run ()
  | _ -> ()

let request_stop () =
  match !current_run with
  | None -> ()
  | Some run -> run.stopped <- true

(* Files left to analyze, kept across restarts so an interrupted run can be
   resumed instead of started over. *)
let pending_key = "superbol.analysis.pending"

let pending_store instance =
  ExtensionContext.workspaceState (Superbol_instance.context instance)

let save_pending instance uris =
  let _ =
    Memento.update (pending_store instance) ~key:pending_key
      ~value:(Jsonoo.t_to_js @@
              Jsonoo.Encode.list Jsonoo.Encode.string @@
              List.map (fun uri -> Uri.toString uri ()) uris)
  in
  ()

let load_pending instance =
  match Memento.get (pending_store instance) ~key:pending_key with
  | None -> []
  | Some value ->
      try
        List.map (fun s -> Uri.parse s ()) @@
        Jsonoo.Decode.(list string) (Jsonoo.t_of_js value)
      with _ -> []

(* Written while the analysis runs, so a crash keeps the results found so far. *)
let create_report ~resume =
  match Workspace.workspaceFolders () with
  | [] -> None
  | folder :: _ ->
      let dir =
        Node.Path.join [Uri.fsPath (WorkspaceFolder.uri folder); "_superbol"]
      in
      let file = Node.Path.join [dir; "analysis-report.txt"] in
      try
        (* The bindings drop the label: this is a plain, non-recursive mkdir,
           and it fails if the directory is already there. *)
        if not (Node.Fs.existsSync dir) then
          Node.Fs.mkdirSync dir ~recursive:true;
        if not (resume && Node.Fs.existsSync file) then
          Node.Fs.writeFileSync file "";
        Some file
      with e ->
        log_line "SuperBOL: cannot create %s: %s" file (Printexc.to_string e);
        None

let append_to_report report text =
  match report with
  | None -> ()
  | Some file ->
      try Node.Fs.appendFileSync file text with e ->
        log_line "SuperBOL: cannot write %s: %s" file (Printexc.to_string e)

let text_document_id uri =
  Jsonoo.Encode.(object_ ["uri", string @@ Uri.toString uri ()])

(* The server handles one message at a time, so a file it never answers would
   block the whole run.  Give up on it instead of waiting for good. *)
let request_timeout_ms = 60_000

type outcome = Analyzed | Skipped | Timed_out

(* The reply only comes once the server has handled the `didOpen' for [uri], so
   waiting for it paces the loop on real work. *)
let await_analysis_of ~uri ~token instance =
  let timer = ref None in
  let request =
    Superbol_instance.lsp_request instance ~token
      ~meth:"textDocument/documentSymbol"
      ~data:Jsonoo.Encode.(object_ ["textDocument", text_document_id uri]) |>
    Promise.then_
      ~fulfilled:(fun _ -> Promise.return Analyzed)
      ~rejected:(fun _ -> Promise.return Analyzed)
  and timeout =
    Promise.make begin fun ~resolve ~reject:_ ->
      timer := Some (Node.setTimeout (fun () -> resolve Timed_out)
                       request_timeout_ms)
    end
  in
  Promise.race_list [request; timeout] |>
  Promise.then_ ~fulfilled:begin fun outcome ->
    Option.iter Node.clearTimeout !timer;
    Promise.return outcome
  end

(* VS Code cannot close a document opened with `openTextDocument', so we
   notify the server ourselves and keep one document in memory at a time. *)
let notify_did_open ~uri ~text instance =
  Superbol_instance.lsp_notification instance
    ~meth:"textDocument/didOpen"
    ~data:Jsonoo.Encode.(object_ [
        "textDocument", object_ [
          "uri", string @@ Uri.toString uri ();
          "languageId", string "cobol";
          "version", int 1;
          "text", string text;
        ];
      ])

let notify_did_close ~uri instance =
  Superbol_instance.lsp_notification instance
    ~meth:"textDocument/didClose"
    ~data:Jsonoo.Encode.(object_ ["textDocument", text_document_id uri])

(* Documents already open are synced by the client: do not close them. *)
let is_already_open uri =
  let uri = Uri.toString uri () in
  List.exists
    (fun doc -> Uri.toString (TextDocument.uri doc) () = uri)
    (Workspace.textDocuments ())

let analyze_document ~uri ~report ~token instance =
  let open Promise.Syntax in
  Promise.catch
    ~rejected:begin fun error ->
      let value =
        Printf.sprintf "%s: skipped, %s"
          (Workspace.asRelativePath () ~pathOrUri:(`Uri uri))
          (Node.JsError.message error)
      in
      log_line "SuperBOL: %s" value;
      append_to_report report (value ^ "\n");
      Promise.return Skipped
    end @@
  if is_already_open uri then
    await_analysis_of ~uri ~token instance
  else
    let* text = Node.Fs.readFile (Uri.fsPath uri) in
    notify_did_open ~uri ~text instance;
    let+ outcome = await_analysis_of ~uri ~token instance in
    (* Close it even on a timeout: the server may still catch up. *)
    notify_did_close ~uri instance;
    outcome

let severity_name = function
  | DiagnosticSeverity.Error -> "error"
  | DiagnosticSeverity.Warning -> "warning"
  | DiagnosticSeverity.Information -> "note"
  | DiagnosticSeverity.Hint -> "hint"

let report_line ~path diag =
  let pos = Range.start @@ Diagnostic.range diag in
  Printf.sprintf "%s:%u:%u: %s: %s\n" path
    (succ @@ Position.line pos) (succ @@ Position.character pos)
    (severity_name @@ Diagnostic.severity diag)
    (Diagnostic.message diag)

let report_diagnostics_of ~uri report =
  match Languages.getDiagnostics uri with
  | [] -> ()
  | diags ->
      let path = Workspace.asRelativePath () ~pathOrUri:(`Uri uri) in
      append_to_report report @@
      String.concat "" @@ List.map (report_line ~path) diags

let report_completion report ~analyzed ~skipped ~missed =
  let outcome =
    if missed = 0 then
      Printf.sprintf "SuperBOL: analyzed %u file(s)" analyzed
    else
      Printf.sprintf "SuperBOL: analysis stopped after %u of %u file(s)"
        (analyzed + skipped) (analyzed + skipped + missed)
  and unread =
    if skipped = 0 then ""
    else Printf.sprintf "; %u file(s) could not be read" skipped
  and where =
    match report with
    | None ->
        "; diagnostics are listed in the Problems view"
    | Some file ->
        Printf.sprintf "; diagnostics are listed in the Problems view and in %s"
          (Workspace.asRelativePath () ~pathOrUri:(`Uri (Uri.file file)))
  in
  let message = outcome ^ unread ^ where in
  append_to_report report (Printf.sprintf "\n%s%s\n" outcome unread);
  let _ =
    match report with
    | None ->
        Window.showInformationMessage () ~message |>
        Promise.then_ ~fulfilled:(fun (_: unit option) -> Promise.return ())
    | Some file ->
        Window.showInformationMessage () ~message
          ~choices:["Show Report", ()] |>
        Promise.then_ ~fulfilled:begin function
          | Some () ->
              let _ =
                Window.showTextDocument ~document:(`Uri (Uri.file file)) ()
              in
              Promise.return ()
          | None ->
              Promise.return ()
        end
  in
  Promise.return ()

(* Saving on every file would mean thousands of writes to the workspace
   state, so a crash may cost the last few files. *)
let save_pending_every = 20

let analyze_workspace instance ~pending ~progress ~token =
  let open Promise.Syntax in
  let* uris =
    match pending with
    | [] -> find_cobol_files ~token
    | uris -> Promise.return uris
  in
  let run = start_run () in
  let report = create_report ~resume:(pending <> []) in
  let total = List.length uris in
  log_line "SuperBOL: analysis started on %u file(s)" total;
  let percent i = i * 100 / max 1 total in
  let stop_requested () =
    run.stopped ||
    CancellationToken.isCancellationRequested token ||
    not (Superbol_instance.client_is_running instance)
  in
  let rec loop i skipped = function
    | remaining when stop_requested () ->
        save_pending instance remaining;
        report_completion report ~analyzed:(i - skipped) ~skipped
          ~missed:(List.length remaining)
    | [] ->
        save_pending instance [];
        report_completion report ~analyzed:(i - skipped) ~skipped ~missed:0
    | uri :: remaining ->
        Progress.report progress ~value:Progress.{
            message = Some (Printf.sprintf "%u/%u: %s" (succ i) total @@
                            Workspace.asRelativePath () ~pathOrUri:(`Uri uri));
            increment = Some (percent (succ i) - percent i);
          };
        let* outcome = analyze_document ~uri ~report ~token instance in
        match outcome with
        | Timed_out ->
            let value =
              Printf.sprintf "%s: the server did not answer within %us, \
                              stopping"
                (Workspace.asRelativePath () ~pathOrUri:(`Uri uri))
                (request_timeout_ms / 1000)
            in
            log_line "SuperBOL: %s" value;
            append_to_report report (value ^ "\n");
            run.stopped <- true;
            loop i skipped (uri :: remaining)   (* keep it in the queue *)
        | Analyzed | Skipped ->
            if outcome = Analyzed then report_diagnostics_of ~uri report;
            if succ i mod save_pending_every = 0 then
              save_pending instance remaining;
            loop (succ i)
              (if outcome = Analyzed then skipped else succ skipped) remaining
  in
  save_pending instance uris;
  let finish () =
    log_line "SuperBOL: analysis ended";
    end_run run
  in
  match loop 0 0 uris with
  | analysis -> Promise.finally ~f:finish analysis
  | exception e -> finish (); raise e

(* The server drops all diagnostics unless `forceSyntaxDiagnostics' is set or
   the dialect is COBOL85 (see `dispatch_diagnostics' in `lsp_server.ml').  The
   dialect is per project, so we warn instead of refusing. *)
let check_diagnostics_reported () =
  if Superbol_workspace.bool "forceSyntaxDiagnostics" ||
     Superbol_workspace.string "cobol.dialect" = "cobol85" then
    Promise.return `Scan
  else
    let open Promise.Syntax in
    let+ choice =
      Window.showWarningMessage ()
        ~message:"SuperBOL only reports diagnostics for projects that use the \
                  COBOL85 dialect, unless `superbol.forceSyntaxDiagnostics' is \
                  enabled.  The scan may find nothing to report."
        ~choices:["Enable and Restart Server", `Enable;
                  "Scan Anyway", `Scan]
    in
    Option.value choice ~default:`Abort

(* Writing the setting restarts the server.  We cannot await that, so we ask
   for a new run. *)
let enable_syntax_diagnostics () =
  let open Promise.Syntax in
  let target =
    if Workspace.workspaceFolders () = []
    then ConfigurationTarget.Global
    else ConfigurationTarget.Workspace
  in
  let+ () =
    WorkspaceConfiguration.update
      (Workspace.getConfiguration ~section:"superbol" ())
      ~section:"forceSyntaxDiagnostics"
      ~value:(Ojs.bool_to_js true)
      ~configurationTarget:(`ConfigurationTarget target) ()
  in
  let _ =
    Window.showInformationMessage ()
      ~message:"Diagnostics enabled.  The language server is restarting; \
                please run the analysis again."
  in
  ()

let scan_workspace ~pending instance =
  Window.withProgress (module Interop.Js.Unit)
    ~options:(ProgressOptions.create
                ~location:(`ProgressLocation ProgressLocation.Notification)
                ~title:"SuperBOL: analyzing COBOL files"
                ~cancellable:true ())
    ~task:(analyze_workspace instance ~pending)

(* A stopped run can be resumed without restarting, so ask here too. *)
let ask_resume instance =
  match load_pending instance with
  | [] ->
      Promise.return []
  | pending ->
      let open Promise.Syntax in
      let+ choice =
        Window.showInformationMessage ()
          ~message:(Printf.sprintf "SuperBOL: %u file(s) were left unanalyzed"
                      (List.length pending))
          ~choices:["Resume", `Resume; "Start Over", `Restart]
      in
      match choice with
      | Some `Resume -> pending
      | Some `Restart | None -> []

let run_analysis ?pending instance =
  match !current_run with
  | Some _ ->
      let open Promise.Syntax in
      let+ choice =
        Window.showWarningMessage ()
          ~message:"SuperBOL: an analysis is already running"
          ~choices:["Stop It", ()]
      in
      begin match choice with
        | Some () -> request_stop (); clear_run ()
        | None -> ()
      end
  | None ->
  match Superbol_instance.client instance with
  | None ->
      Superbol_printer.show_error_message @@
      Error Superbol_types.Client_not_running
  | Some _ ->
      let open Promise.Syntax in
      let* pending =
        match pending with
        | Some pending -> Promise.return pending
        | None -> ask_resume instance
      in
      let* decision = check_diagnostics_reported () in
      match decision with
      | `Abort -> Promise.return ()
      | `Enable -> enable_syntax_diagnostics ()
      | `Scan -> scan_workspace ~pending instance

let _analyze_workspace =
  command "superbol.analyze.workspace" @@ Instance
    begin fun instance ~args:_ ->
      let _: unit Promise.t = run_analysis instance in
      ()
    end

let _stop_analysis =
  command "superbol.analyze.stop" @@ Instance
    begin fun _instance ~args:_ ->
      request_stop ()
    end

(* Dismissing keeps the files pending, so the offer comes back next time. *)
let offer_resume instance =
  match load_pending instance with
  | [] ->
      Promise.return ()
  | pending ->
      let open Promise.Syntax in
      let* choice =
        Window.showInformationMessage ()
          ~message:(Printf.sprintf
                      "SuperBOL: a previous analysis left %u file(s) \
                       unanalyzed" (List.length pending))
          ~choices:["Resume", `Resume; "Discard", `Discard]
      in
      match choice with
      | Some `Resume -> run_analysis ~pending instance
      | Some `Discard -> save_pending instance []; Promise.return ()
      | None -> Promise.return ()

(** {2 Copybook directory retrieval} *)

(* Extensions used to look up copybooks.  Lowercased like the server does. *)
let copybook_extensions () =
  List.map String.lowercase_ascii @@
  Superbol_workspace.strings Superbol_tasks.copyexts_setting

let copybook_file_pattern exts =
  Printf.sprintf "**/*.{%s}" @@
  String.concat "," @@
  List.concat_map (fun ext -> [ext; String.uppercase_ascii ext]) exts

module Json_list = Interop.Js.List (Jsonoo)

let superbol_config () =
  Workspace.getConfiguration ~section:"superbol" ()

(* Workspace value only: the default and the user-wide value must not be
   copied into the workspace settings.  Malformed entries are skipped. *)
let configured_copybook_dirs () =
  match
    WorkspaceConfiguration.inspect (module Json_list) (superbol_config ())
      ~section:Superbol_tasks.copybooks_setting
  with
  | Some { WorkspaceConfiguration.workspaceValue = Some entries; _ } ->
      List.filter_map
        (Jsonoo.Decode.try_optional Superbol_tasks.copybook_path_of_jsonoo)
        entries
  | _ ->
      []

let find_copybook_files exts =
  Workspace.findFiles () ~includes:(`String (copybook_file_pattern exts))

(* `asRelativePath' always uses "/", on every platform.  It gives back the
   whole path for a file that is outside the workspace folder. *)
let relative_dir_of uri =
  Filename.dirname @@
  Workspace.asRelativePath ~pathOrUri:(`Uri uri) ~includeWorkspaceFolder:false ()

(* A file-relative entry covers any directory ending with it, like the
   server's `file_is_in_libpath'. *)
let covered_by existing dir =
  List.exists begin fun Superbol_tasks.{ dir = d; file_relative } ->
    if file_relative then String.ends_with ~suffix:d dir else d = dir
  end existing

let missing_dirs ~root_fs existing =
  List.filter begin fun Superbol_tasks.{ dir; file_relative } ->
    not file_relative &&
    not (Node.Fs.existsSync (Node.Path.join [root_fs; dir]))
  end existing

let plural n one many = if n = 1 then one else many

let report_retrieval ~added ~total ~missing =
  let outcome =
    if added = 0 then
      Printf.sprintf "SuperBOL: copybook paths are already up to date \
                      (%u director%s configured)"
        total (plural total "y" "ies")
    else
      Printf.sprintf "SuperBOL: added %u copybook director%s to the \
                      workspace settings"
        added (plural added "y" "ies")
  and stale =
    match List.length missing with
    | 0 -> ""
    | n -> Printf.sprintf "; %u configured director%s no longer exist%s"
             n (plural n "y" "ies") (plural n "s" "")
  in
  let message = outcome ^ stale in
  (* Do not wait for the answer: the message only closes when the user acts on
     it, and the progress notification would stay up until then. *)
  let _ =
    Window.showInformationMessage () ~message ~choices:["Show Settings", ()] |>
    Promise.then_ ~fulfilled:begin function
      | Some () ->
          let _ =
            Commands.executeCommand ~args:[]
              ~command:"workbench.action.openWorkspaceSettingsFile"
          in
          Promise.return ()
      | None ->
          Promise.return ()
    end
  in
  Promise.return ()

let retrieve_copybook_dirs ~exts ~root_fs =
  let open Promise.Syntax in
  let* uris = find_copybook_files exts in
  let existing = configured_copybook_dirs () in
  let missing = missing_dirs ~root_fs existing in
  let found =
    List.sort_uniq String.compare @@ List.map relative_dir_of uris
  in
  let added =
    List.filter_map begin fun dir ->
      if covered_by existing dir
      then None
      else Some Superbol_tasks.{ dir; file_relative = false }
    end found
  in
  if found = [] then begin
    let _ =
      Window.showInformationMessage ()
        ~message:(Printf.sprintf
                    "SuperBOL: no copybook found in the workspace (looking \
                     for %s files)" @@
                  String.concat ", " @@
                  List.map (fun ext -> "`." ^ ext ^ "'") exts)
    in
    Promise.return ()
  end else if added = [] then
    report_retrieval ~added:0 ~total:(List.length existing) ~missing
  else
    let* () =
      WorkspaceConfiguration.update (superbol_config ())
        ~section:Superbol_tasks.copybooks_setting
        ~value:(Jsonoo.t_to_js @@
                Jsonoo.Encode.list Superbol_tasks.copybook_path_to_jsonoo
                  (existing @ added))
        ~configurationTarget:
          (`ConfigurationTarget ConfigurationTarget.Workspace) ()
    in
    report_retrieval ~added:(List.length added)
      ~total:(List.length existing + List.length added) ~missing

(* Relative paths resolve against the server's working directory, ie. the
   first workspace folder.  Only safe with a single folder. *)
let run_copybook_retrieval () =
  match Workspace.workspaceFolders () with
  | [] ->
      let _ =
        Window.showWarningMessage ()
          ~message:"SuperBOL: open a folder before retrieving copybook \
                    directories"
      in
      Promise.return ()
  | _ :: _ :: _ ->
      let _ =
        Window.showWarningMessage ()
          ~message:"SuperBOL: copybook retrieval is not supported in \
                    multi-root workspaces yet"
      in
      Promise.return ()
  | [folder] ->
      match copybook_extensions () with
      | [] ->
          let _ =
            Window.showWarningMessage ()
              ~message:"SuperBOL: no extension listed in \
                        `superbol.cobol.copyexts'"
          in
          Promise.return ()
      | exts ->
          retrieve_copybook_dirs ~exts
            ~root_fs:(Uri.fsPath @@ WorkspaceFolder.uri folder)

let _retrieve_copybooks =
  command "superbol.copybooks.retrieve" @@ Instance
    begin fun _instance ~args:_ ->
      let _: unit Promise.t = run_copybook_retrieval () in
      ()
    end

let _restart_language_server =
  command "superbol.server.restart" @@ Instance
    begin fun instance ~args:_ ->
      let _: unit Promise.t =
        Superbol_instance.start_language_server instance
      in
      ()
    end

let _write_project_config =
  command "superbol.write.project.config" @@ Instance
    begin fun instance ~args:_ ->
      let _: unit Promise.t =
        Superbol_instance.write_project_config instance
      in ()
    end

let _show_coverage =
  command "superbol.coverage.show" @@ Instance
    (fun _instance ~args:_ ->
      let _ =
        Commands.executeCommand ~command:"gcov-viewer.show" ~args:[]
      in
      ())

let _hide_coverage =
  command "superbol.coverage.hide" @@ Instance
    (fun _instance ~args:_ ->
      let _ =
        Commands.executeCommand ~command:"gcov-viewer.hide" ~args:[]
      in
      ())

let _reload_coverage =
  command "superbol.coverage.reload" @@ Instance
    (fun _ ~args:_ ->
      let _ =
        Commands.executeCommand ~command:"gcov-viewer.reloadGcdaFiles" ~args:[]
      in
      ())

let register extension instance { id; handler } =
  match handler with
  | Instance callback ->
      let callback = callback instance in
      ExtensionContext.subscribe extension
        ~disposable:(Commands.registerCommand ~command:id ~callback)
  (* | Text callback -> *)
  (*     let callback = callback instance in *)
  (*     ExtensionContext.subscribe extension *)
  (*       ~disposable:(Commands.registerTextEditorCommand ~command:id ~callback) *)

let register_all extension instance =
  List.iter (register extension instance) !commands
