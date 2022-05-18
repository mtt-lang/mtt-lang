open Base
open Result.Let_syntax
open ParserInterface

(* Parsing with error handling utilities *)
let parse_from_e : type a. a ast_kind -> input_kind -> (a, error) Result.t =
 fun ast_kind source ->
  parse_from ast_kind source
  |> Result.map_error ~f:(fun parse_error ->
         [%string "Parse error: $parse_error"])

(* Parsing and typechecking with error handling utilities *)
let parse_and_typecheck source typ_str =
  let%bind ast = parse_from_e Prog source in
  let%bind typ = parse_from_e Type (String typ_str) in
  Typechecker.check ast typ
  |> Result.map_error ~f:(fun infer_err ->
         [%string "Typechecking error: $(MttError.located_to_string infer_err)"])

(* Parsing and type inference with error handling utilities *)
let parse_and_typeinfer source =
  let%bind ast = parse_from_e Prog source in
  Typechecker.infer ast
  |> Result.map_error ~f:(fun infer_err ->
         [%string
           "Type inference error: $(MttError.located_to_string infer_err)"])

(* Parsing and evaluation with error handling utilities *)
let parse_and_eval source =
  let%bind ast = parse_from_e Prog source in
  Evaluator.eval ast
  |> Result.map_error ~f:(fun eval_err ->
         [%string "Evaluation error: $(MttError.to_string eval_err)"])
