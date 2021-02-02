open Js_of_ocaml
module Html = Dom_html 

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
  output_to_terminal terminal content

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
