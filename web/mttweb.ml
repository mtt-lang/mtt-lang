(* start copy-paste from ../bin/mtt.ml *)
open Mtt
(* caml_failwith(\n[ ]+)?\("Base(.*)"\) *)
open Base
open ParserInterface

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

(* end copy-paste from ../bin/mtt.ml *)


open Js_of_ocaml
module Html = Dom_html

let eval_web term =
  match parse_and_eval term with
  | Ok res  -> PrettyPrinter.Doc.of_val res
  | Error _ -> assert false

(* create editor *)
let editor_create _ =
  let editor = Js.Unsafe.new_obj (Js.Unsafe.variable "ace.edit") [| Js.Unsafe.inject (Js.string "editor") |] in
  let editor_options = Json.unsafe_input @@ Js.string
    "{
      \"fontSize\"                 : \"20px\",
      \"selectionStyle\"           : \"text\",
      \"autoScrollEditorIntoView\" : true,
      \"showPrintMargin\"          : false
    }"
  in
  let _ = Js.Unsafe.meth_call editor "setOptions" [| Js.Unsafe.inject editor_options |] in
  let _ = Js.Unsafe.meth_call editor "setTheme" [| Js.Unsafe.inject @@ Js.string "ace/theme/twilight" |] in
  let _ = Js.Unsafe.meth_call (editor##.session) "setMode" [| Js.Unsafe.inject @@ Js.string "ace/mode/mtt" |] in
  editor;;

(* create terminal  *)
let terminal_create _ =
  let terminal = Js.Unsafe.new_obj (Js.Unsafe.variable "ace.edit") [| Js.Unsafe.inject (Js.string "terminal") |] in
  let terminal_options = Json.unsafe_input @@ Js.string
    "{
      \"fontSize\" : \"14px\",
      \"copyWithEmptySelection\" : true,
      \"readOnly\" : true
    }"
  in
  let _ = Js.Unsafe.meth_call terminal "setOptions" [| Js.Unsafe.inject terminal_options |] in
  let _ = Js.Unsafe.meth_call terminal "setTheme" [| Js.Unsafe.inject @@ Js.string "ace/theme/twilight" |] in
  terminal;;

(* print result to terminal *)
let output_to_terminal terminal msg =
  terminal##setValue(msg);
  terminal##clearSelection

(* action when `eval` button pressed *)
let eval_onclick editor terminal _ =
  let content = editor##getValue in
  let ik = eval_web content in
  output_to_terminal terminal ik

(* action when `infer` button pressed *)
let infer_onclick editor terminal _ =
  let content = editor##getValue in
  output_to_terminal terminal content

(* action when `typecheck` button pressed *)
let typecheck_onclick editor terminal _ =
  let content = editor##getValue in
  output_to_terminal terminal content

(* language description and examples *)
let create_east_content _ =
  let content = Dom_html.getElementById "content" in
  let document = Html.window##.document in
  Dom.appendChild content (document##createTextNode (Js.string "MTT description"))

let start _ =
  let editor = editor_create () in
  let terminal = terminal_create () in
  Js.Unsafe.global##.jsEval := Js.wrap_callback (eval_onclick editor terminal);
  Js.Unsafe.global##.jsInfer := Js.wrap_callback (infer_onclick editor terminal);
  Js.Unsafe.global##.jsTypecheck := Js.wrap_callback (typecheck_onclick editor terminal);
  create_east_content();
  Js._false;;

let _ = Html.window##.onload := Html.handler start
