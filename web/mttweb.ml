(* start copy-paste from ../bin/mtt.ml *)
open Base

open Mtt
open ParserInterface
open Result.Let_syntax

let parse_from_e : type a. a ast_kind -> input_kind -> (a, error) Result.t =
 fun ast_kind source ->
  parse_from ast_kind source
  |> Result.map_error ~f:(fun parse_error ->
         [%string "Parse error: $parse_error"])

let parse_and_eval source =
  let open Result.Let_syntax in
  let%bind ast = parse_from_e Term source in
  Evaluator.eval ast
  |> Result.map_error ~f:(fun eval_err ->
         [%string "Evaluation error: $eval_err"])

let parse_and_typeinfer source =
  let%bind ast = parse_from_e Term source in
  Typechecker.infer ast
  |> Result.map_error ~f:(fun eval_err ->
         [%string "Type inference error: $eval_err"])

(* end copy-paste from ../bin/mtt.ml *)

let eval_web term =
  match parse_and_eval term with
  | Ok res ->
      let document = PrettyPrinter.Doc.of_val res in
      (* Check buffer size *)
      let buffer = Buffer.create 100 in
      PPrint.ToBuffer.pretty 1.0 80 buffer document;
      Buffer.contents buffer
  | Error err_msg -> err_msg

let infer_web term =
  match parse_and_typeinfer term with
  | Ok res ->
      let document = PrettyPrinter.Doc.of_type res in
      (* Check buffer size *)
      let buffer = Buffer.create 100 in
      PPrint.ToBuffer.pretty 1.0 80 buffer document;
      Buffer.contents buffer
  | Error err_msg -> err_msg

open Js_of_ocaml
module Html = Dom_html

(* create editor *)
let editor_create _ =
  let editor =
    Js.Unsafe.new_obj
      (Js.Unsafe.variable "ace.edit")
      [| Js.Unsafe.inject (Js.string "editor") |]
  in
  let editor_options =
    Json.unsafe_input
    @@ Js.string
         "{\n\
         \      \"fontSize\"                 : \"20px\",\n\
         \      \"selectionStyle\"           : \"text\",\n\
         \      \"autoScrollEditorIntoView\" : true,\n\
         \      \"showPrintMargin\"          : false\n\
         \    }"
  in
  let _ =
    Js.Unsafe.meth_call editor "setOptions"
      [| Js.Unsafe.inject editor_options |]
  in
  let _ =
    Js.Unsafe.meth_call editor "setTheme"
      [| Js.Unsafe.inject @@ Js.string "ace/theme/twilight" |]
  in
  let _ =
    Js.Unsafe.meth_call editor##.session "setMode"
      [| Js.Unsafe.inject @@ Js.string "ace/mode/mtt" |]
  in
  editor

(* create terminal  *)
let terminal_create _ =
  let terminal =
    Js.Unsafe.new_obj
      (Js.Unsafe.variable "ace.edit")
      [| Js.Unsafe.inject (Js.string "terminal") |]
  in
  let terminal_options =
    Json.unsafe_input
    @@ Js.string
         "{\n\
         \      \"fontSize\" : \"14px\",\n\
         \      \"copyWithEmptySelection\" : true,\n\
         \      \"readOnly\" : true\n\
         \    }"
  in
  let _ =
    Js.Unsafe.meth_call terminal "setOptions"
      [| Js.Unsafe.inject terminal_options |]
  in
  let _ =
    Js.Unsafe.meth_call terminal "setTheme"
      [| Js.Unsafe.inject @@ Js.string "ace/theme/twilight" |]
  in
  terminal

(* print result to terminal *)
let output_to_terminal terminal msg =
  terminal##setValue msg;
  terminal##clearSelection

(* action when `eval` button pressed *)
let eval_onclick editor terminal _ =
  let content = Js.to_string editor##getValue in
  let result = Js.string @@ eval_web (String content) in
  output_to_terminal terminal result

(* action when `infer` button pressed *)
let infer_onclick editor terminal _ =
  let content = Js.to_string editor##getValue in
  let result = Js.string @@ infer_web (String content) in
  output_to_terminal terminal result

(* action when `typecheck` button pressed *)
let clear_onclick editor _ = editor##setValue (Js.string "")

(* language description and examples *)
let create_east_content _ =
  let content = Dom_html.getElementById "content" in
  let document = Html.window##.document in
  Dom.appendChild content
    (document##createTextNode (Js.string "MTT description"))

let start _ =
  let editor = editor_create () in
  let terminal = terminal_create () in
  Js.Unsafe.global##.jsEval := Js.wrap_callback (eval_onclick editor terminal);
  Js.Unsafe.global##.jsInfer := Js.wrap_callback (infer_onclick editor terminal);
  Js.Unsafe.global##.jsClear := Js.wrap_callback (clear_onclick editor);
  create_east_content ();
  Js._false

let _ = Html.window##.onload := Html.handler start
