(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Vscode

(** {2 Program used to analyze the copybook in the active editor}

    A copybook is not analyzed on its own: the server uses the last program it
    analyzed that copies it.  This module shows that program in the status bar,
    and warns when there is none. *)

let no_program_message =
  "SuperBOL: no analyzed program copies this copybook, so diagnostics and \
   hover are not available in it.  Open a program that copies it."

(* Opening the program is enough: the client sends it to the server, which
   analyzes it and then links it with the copybooks it copies. *)
let open_program () =
  let open Promise.Syntax in
  (* Same files as the workspace analysis, so copybooks are listed too: the
     extension of a file does not tell whether it is a program. *)
  let* uris = Superbol_workspace.find_cobol_files () in
  let choices =
    List.map (fun (label, uri) -> QuickPickItem.create () ~label, uri) @@
    List.sort (fun (a, _) (b, _) -> String.compare a b) @@
    List.map begin fun uri ->
      Workspace.asRelativePath () ~pathOrUri:(`Uri uri), uri
    end uris
  in
  match choices with
  | [] ->
      let _ =
        Window.showWarningMessage ()
          ~message:"SuperBOL: no COBOL file found in the workspace"
      in
      Promise.return ()
  | choices ->
      let* choice =
        Window.showQuickPickItems () ~choices
          ~options:(QuickPickOptions.create ()
                      ~title:"Select a program that copies this copybook")
      in
      match choice with
      | None ->
          Promise.return ()
      | Some uri ->
          let+ _ = Window.showTextDocument () ~document:(`Uri uri) in
          ()

(* Only warn once per copybook, so that switching editors is not noisy. *)
let warned = ref []

let warn_about uri =
  let key = Uri.toString uri () in
  if not (List.mem key !warned) then begin
    warned := key :: !warned;
    let _ =
      Window.showWarningMessage () ~message:no_program_message
        ~choices:["Open Program…", ()] |>
      Promise.then_ ~fulfilled:begin function
        | Some () ->
            (* Warn again in case the opened program does not copy it. *)
            warned := List.filter (fun u -> u <> key) !warned;
            open_program ()
        | None ->
            Promise.return ()
      end
    in
    ()
  end

let show_program item program =
  let uri = Uri.parse program () in
  StatusBarItem.set_text item @@
  Printf.sprintf "$(file-code) %s"
    (Workspace.asRelativePath () ~pathOrUri:(`Uri uri));
  StatusBarItem.set_tooltip item @@
  Printf.sprintf "Program used by SuperBOL to analyze this copybook: %s"
    (Uri.fsPath uri);
  (* A click opens the program. *)
  StatusBarItem.set_command item @@
  `Command (Command.create () ~title:"Open Program" ~command:"vscode.open"
              ~arguments:[Uri.t_to_js uri]);
  StatusBarItem.show item

(* The reply tells whether the document is a copybook, and which program is
   used for it. *)
let show_reply item ~uri reply =
  match
    Jsonoo.Decode.(field "copybook" bool) reply,
    Jsonoo.Decode.(field "program" (nullable string)) reply
  with
  | false, _ ->
      StatusBarItem.hide item
  | true, Some program ->
      show_program item program
  | true, None ->
      StatusBarItem.hide item;
      warn_about uri
  | exception Jsonoo.Decode_error _ ->
      StatusBarItem.hide item

(* Created when the extension starts. *)
let status_bar_item = ref None

(** Shows the program used for the document in the active editor. *)
let update instance =
  match !status_bar_item, Superbol_instance.current_document_uri () with
  | None, _ ->
      ()
  | Some item, None ->
      StatusBarItem.hide item
  | Some item, Some uri ->
      let _ =
        Superbol_instance.lsp_request instance
          ~meth:"superbol/getReferenceProgram"
          ~data:Jsonoo.Encode.(object_ ["uri", string @@ Uri.toString uri ()]) |>
        Promise.then_ ~fulfilled:begin function
          | Ok reply -> Promise.return @@ show_reply item ~uri reply
          | Error _ -> Promise.return @@ StatusBarItem.hide item
        end
      in
      ()

let start_status_bar instance =
  let item =
    Window.createStatusBarItem ~alignment:StatusBarAlignment.Right ()
  in
  status_bar_item := Some item;
  Superbol_instance.subscribe_disposable instance
    (StatusBarItem.disposable item);
  Superbol_instance.subscribe_disposable instance @@
  Window.onDidChangeActiveTextEditor () ~listener:(fun _ -> update instance) ();
  update instance
