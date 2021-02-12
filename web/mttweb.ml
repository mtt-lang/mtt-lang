(* start copy-paste from ../bin/mtt.ml *)
open Base
open Mtt
open ParserInterface
open Result.Let_syntax

let format_located_error Location.{ data = error; loc } =
  match error with
  | `TypeMismatchError msg -> Location.pp ~msg loc
  | `EvaluationError msg -> Location.pp ~msg loc
  | `EnvUnboundVariableError (_, msg) -> Location.pp ~msg loc
  | `UnboundRegularVarInsideBoxError (_, msg) -> Location.pp ~msg loc

let format_error error =
  match error with
  | `EvaluationError msg -> msg
  | `EnvUnboundVariableError (_, msg) -> msg
  | `TypeMismatchError msg -> msg

let parse_from_e : type a. a ast_kind -> input_kind -> (a, error) Result.t =
 fun ast_kind source ->
  parse_from ast_kind source
  |> Result.map_error ~f:(fun parse_error ->
         [%string "Parse error: $parse_error"])

let parse_and_typeinfer source =
  let%bind ast = parse_from_e Term source in
  Typechecker.infer ast
  |> Result.map_error ~f:(fun infer_err ->
         [%string "Type inference error: $(format_located_error infer_err)"])

let parse_and_eval source =
  let open Result.Let_syntax in
  let%bind ast = parse_from_e Term source in
  Evaluator.eval ast
  |> Result.map_error ~f:(fun eval_err ->
         [%string "Evaluation error: $(format_error eval_err)"])

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
let create_editor div =
  let editor = Ace.create_editor div in
  Ace.set_theme editor "ace/theme/twilight";
  Ace.set_mode editor "ace/mode/mtt";
  let editor_options =
    "{\n\
    \      \"fontSize\"                 : \"20px\",\n\
    \      \"selectionStyle\"           : \"text\",\n\
    \      \"autoScrollEditorIntoView\" : true,\n\
    \      \"showPrintMargin\"          : false\n\
    \    }"
  in
  Ace.set_options editor editor_options;
  editor

(* create terminal  *)
let create_terminal div =
  let terminal = Ace.create_editor div in
  Ace.set_theme terminal "ace/theme/twilight";
  let terminal_options =
    "{\n\
    \      \"fontSize\" : \"14px\",\n\
    \      \"copyWithEmptySelection\" : true,\n\
    \      \"readOnly\" : true\n\
    \    }"
  in
  Ace.set_options terminal terminal_options;
  terminal

(* print result to terminal *)
let output_to_terminal terminal msg =
  Ace.set_value terminal msg;
  Ace.clear_selection terminal

(* action when `eval` button pressed *)
let eval_onclick editor terminal _ =
  let content = Ace.get_value editor in
  let result = eval_web (String content) in
  output_to_terminal terminal result

(* action when `infer` button pressed *)
let infer_onclick editor terminal _ =
  let content = Ace.get_value editor in
  let result = infer_web (String content) in
  output_to_terminal terminal result

(* action when `clear` button pressed *)
let clear_onclick editor _ = Ace.set_value editor ""

(* wrapper for `loadfile` from script.js *)
let loadfile (editor : Ace.editor) (filename : filename) : unit =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "loadfile")
    [| Js.Unsafe.inject editor; Js.Unsafe.inject @@ Js.string filename |]

let load_files (editor : Ace.editor) (l : filename list) : unit =
  let open Js_of_ocaml_tyxml.Tyxml_js in
  let elem s =
    Html.(
      li
        [
          a
            ~a:
              [
                a_class [ "file" ];
                a_href ("#" ^ s);
                a_title s;
                a_onclick (fun _ ->
                    loadfile editor s;
                    false);
              ]
            [ txt s ];
        ])
  in
  let l = Html.ul (List.map l ~f:elem) in
  Register.id ~keep:true "examples" [ l ]

(* language description and examples *)
let create_east_content _ =
  let content = Html.getElementById "content" in
  let document = Html.window##.document in
  let description =
    Js.string
      "Welcome to the online demo of the Mtt language, a functional langauge \
       based on a modal type theory.\n\
      \     Beware, this is a prototype: UI in general are research-quality.\n\
      \     You can find a list of examples below.\n\
      \    "
  in
  Dom.appendChild content (document##createTextNode description)

let start _ =
  let editor = create_editor "editor" in
  let terminal = create_terminal "terminal" in
  Js.Unsafe.global##.jsEval := Js.wrap_callback (eval_onclick editor terminal);
  Js.Unsafe.global##.jsInfer := Js.wrap_callback (infer_onclick editor terminal);
  Js.Unsafe.global##.jsClear := Js.wrap_callback (clear_onclick editor);
  let files =
    [
      "apply.mtt";
      "boxed-id.mtt";
      "boxed-product-curried.mtt";
      "boxed-product1.mtt";
      "boxed-product2.mtt";
      "eval-apply.mtt";
      "eval.mtt";
      "quote.mtt";
    ]
  in
  load_files editor files;
  create_east_content ();
  Js._false

let _ = Html.window##.onload := Html.handler start
