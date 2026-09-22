(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Lsp_testing

(** Tests for copybook detection; very basic for now *)

let make_lsp_project_with_global_copybook_dir dir =
  make_lsp_project () ~toml:(Pretty.to_string {toml|
    cobol.copybooks = [{ dir = %S, file-relative = false }]
  |toml} dir)

let make_lsp_project_with_file_relative_copybook_dir dir =
  make_lsp_project () ~toml:(Pretty.to_string {toml|
    cobol.copybooks = [{ dir = %S, file-relative = true }]
  |toml} dir)

(* --- *)

let%expect_test "typical-copybook" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  ignore @@ add_cobol_doc server ~projdir "FIELD" {cobol|
       01 FIELD PIC X.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"message":"file://__rootdir__/FIELD appears to be a copybook","type":4},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/FIELD"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}];;


let%expect_test "typical-program" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  ignore @@ add_cobol_doc server ~projdir "PROGRAM" {cobol|
       program-id. prog.
       procedure division.
          stop run.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/PROGRAM"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}];;

let%expect_test "typical-copybook-in-project-relative-copybook-dir" =
  let { projdir; end_with_postproc }, server =
    make_lsp_project_with_global_copybook_dir "global/copybooks"
  in
  ignore @@ add_cobol_doc server ~projdir "global/copybooks/FIELD" {cobol|
       01 FIELD PIC X.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"message":"file://__rootdir__/global/copybooks/FIELD appears to be a copybook","type":4},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/global/copybooks/FIELD"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}];;


let%expect_test "weird-copybook-in-project-relative-copybook-dir" =
  let { projdir; end_with_postproc }, server =
    make_lsp_project_with_global_copybook_dir "global/copybooks"
  in
  ignore @@ add_cobol_doc server ~projdir "global/copybooks/INDIRCPY" {cobol|
       COPY X.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"message":"file://__rootdir__/global/copybooks/INDIRCPY appears to be a copybook","type":4},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/global/copybooks/INDIRCPY"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}];;


let%expect_test "typical-copybook-in-file-relative-copybook-dir" =
  let { projdir; end_with_postproc }, server =
    make_lsp_project_with_file_relative_copybook_dir "lib/copybooks"
  in
  ignore @@ add_cobol_doc server ~projdir "lib/copybooks/FIELD" {cobol|
       01 FIELD PIC X.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"message":"file://__rootdir__/lib/copybooks/FIELD appears to be a copybook","type":4},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/lib/copybooks/FIELD"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}];;


(* --- *)

(** Tests for the program that is used to analyze an opened copybook *)

let reference_program server ~copybook:{ Lsp.Types.TextDocumentIdentifier.uri } =
  match LSP.Server.reference_program ~uri server with
  | None -> Pretty.out "No reference program@."
  | Some doc -> Pretty.out "Reference program: %s@."
                  (Lsp.Uri.to_string @@ LSP.Document.uri doc)

(* The client asks for the program via this request, to show it to the user. *)
let get_reference_program server
    ~copybook:{ Lsp.Types.TextDocumentIdentifier.uri } =
  let request =
    Jsonrpc.Request.create () ~id:(`Int 1)
      ~method_:"superbol/getReferenceProgram"
      ~params:(`Assoc ["uri", `String (Lsp.Uri.to_string uri)])
  in
  match LSP.Request.handle request (LSP.Types.Running server) with
  | _, { result = Ok json; _ } -> Pretty.out "%s@." (Yojson.Safe.to_string json)
  | _, { result = Error { message; _ }; _ } -> Pretty.out "Error: %s@." message

let hover_in server ~copybook ~line ~character =
  let params = Lsp.Types.HoverParams.create ~textDocument:copybook
      ~position:(Lsp.Types.Position.create ~line ~character) () in
  match LSP.Request.INTERNAL.hover server params with
  | None ->
      Pretty.out "Hovering nothing worthy@."
  | Some { contents = `List strings; _ } ->
      List.iter (fun Lsp.Types.MarkedString.{ value; _ } -> print_endline value)
        strings
  | Some { contents = `MarkedString Lsp.Types.MarkedString.{ value; _ } |
                      `MarkupContent Lsp.Types.MarkupContent.{ value; _ }; _ } ->
      print_endline value

let%expect_test "copybook-without-reference-program" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  let server, copybook = add_cobol_doc server ~projdir "lib.cpy" {cobol|
       01 FIELD PIC X.
  |cobol} in
  reference_program server ~copybook;
  get_reference_program server ~copybook;
  hover_in server ~copybook ~line:1 ~character:11;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"message":"file://__rootdir__/lib.cpy appears to be a copybook","type":4},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/lib.cpy"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    No reference program
    {"copybook":true,"program":null}
    Hovering nothing worthy |}];;

let%expect_test "copybook-with-reference-program" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  let server, copybook = add_cobol_doc server ~projdir "lib.cpy" {cobol|
       01 FIELD PIC X.
  |cobol} in
  let server, _ = add_cobol_doc server ~projdir "prog.cob" {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "lib.cpy".
       PROCEDURE DIVISION.
          DISPLAY FIELD
          STOP RUN.
  |cobol} in
  reference_program server ~copybook;
  get_reference_program server ~copybook;
  hover_in server ~copybook ~line:1 ~character:11;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"message":"file://__rootdir__/lib.cpy appears to be a copybook","type":4},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/lib.cpy"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    Reference program: file://__rootdir__/prog.cob
    {"copybook":true,"program":"file://__rootdir__/prog.cob"}
    Offset: 0 bytes
    Size: 1 byte
    ---
    References: 2 |}];;

let%expect_test "copybook-with-closed-reference-program" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  let copybook_text = {cobol|
       01 FIELD PIC X.
  |cobol} in
  let server, _ = add_cobol_doc server ~projdir "lib.cpy" copybook_text in
  let server, prog = add_cobol_doc server ~projdir "prog.cob" {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "lib.cpy".
       PROCEDURE DIVISION.
          DISPLAY FIELD
          STOP RUN.
  |cobol} in
  let server =
    LSP.Server.did_close
      (Lsp.Types.DidCloseTextDocumentParams.create ~textDocument:prog) server
  in
  (* The program is analyzed again when the copybook is opened. *)
  let server, copybook = add_cobol_doc server ~projdir "lib.cpy" copybook_text in
  reference_program server ~copybook;
  hover_in server ~copybook ~line:1 ~character:11;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"message":"file://__rootdir__/lib.cpy appears to be a copybook","type":4},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/lib.cpy"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/lib.cpy"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    Reference program: file://__rootdir__/prog.cob
    Offset: 0 bytes
    Size: 1 byte
    ---
    References: 2 |}];;
