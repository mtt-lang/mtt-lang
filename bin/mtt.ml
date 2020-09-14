open Core
open Result.Let_syntax
open Stdio
open Mtt
open ParserInterface

(* Parsing with error handling utilities *)
let parse_from_e : type a. a ast_kind -> input_kind -> (a, error) Result.t =
 fun ast_kind source ->
  parse_from ast_kind source
  |> Result.map_error ~f:(fun parse_error ->
         [%string "Parse error: $parse_error"])

(* Parsing and typechecking with error handling utilities *)
let parse_and_typecheck_string expr_str typ_str =
  let%bind ast = parse_from_e Term (String expr_str) in
  let%bind typ = parse_from_e Type (String typ_str) in
  Typechecker.check ast typ
  |> Result.map_error ~f:(fun eval_err ->
         [%string "Typechecking error: $eval_err"])

(* Parsing and type inference with error handling utilities *)
let parse_and_typeinfer source =
  let%bind ast = parse_from_e Term source in
  Typechecker.infer ast
  |> Result.map_error ~f:(fun eval_err ->
         [%string "Type inference error: $eval_err"])

(* Parsing and evaluation with error handling utilities *)
let parse_and_eval source =
  let open Result.Let_syntax in
  let%bind ast = parse_from_e Term source in
  Evaluator.eval ast
  |> Result.map_error ~f:(fun eval_err ->
         [%string "Evaluation error: $eval_err"])

(* Printing results *)

let print_parsing_result res =
  match res with
  | Ok ast ->
      let document = PrettyPrinter.Doc.of_expr ast in
      PPrint.ToChannel.pretty 1.0 80 stdout document;
      Out_channel.newline stdout
  | Error err_msg ->
      Out_channel.output_string stderr err_msg;
      Out_channel.newline stderr

let print_typecheck_result res =
  match res with
  | Ok () ->
      Out_channel.output_string stdout "OK. Term typechecks!";
      Out_channel.newline stdout
  | Error err_msg ->
      Out_channel.output_string stderr err_msg;
      Out_channel.newline stderr

let print_typeinfer_result res =
  match res with
  | Ok typ ->
      let document = PrettyPrinter.Doc.of_type typ in
      PPrint.ToChannel.pretty 1.0 80 stdout document;
      Out_channel.newline stdout
  | Error err_msg ->
      Out_channel.output_string stderr err_msg;
      Out_channel.newline stderr

let print_eval_result res =
  match res with
  | Ok literal ->
      let document = PrettyPrinter.Doc.of_lit literal in
      PPrint.ToChannel.pretty 1.0 80 stdout document;
      Out_channel.newline stdout
  | Error err_msg ->
      Out_channel.output_string stderr err_msg;
      Out_channel.newline stderr

(* Command-line interface *)

let input_kind_of_filename filename =
  match filename with "-" -> Stdin | filename -> File filename

let parse =
  Command.basic ~summary:"Parsing and printing back"
    Command.Let_syntax.(
      let%map_open expr_opt =
        flag "-e" (optional string)
          ~doc:"string Parse and print back the given [expression]"
      and filename =
        anon (maybe_with_default "-" ("filename" %: Filename.arg_type))
      in
      fun () ->
        match expr_opt with
        | Some expr -> parse_from_e Term (String expr) |> print_parsing_result
        | None ->
            filename |> input_kind_of_filename |> parse_from_e Term
            |> print_parsing_result)

let check =
  Command.basic ~summary:"Typechecking"
    Command.Let_syntax.(
      let%map_open expr_opt =
        flag "-e" (optional string) ~doc:"string [expression] to typecheck"
      and type_opt =
        flag "-t" (optional string) ~doc:"string [type] to typecheck"
      in
      fun () ->
        match (expr_opt, type_opt) with
        | Some expr, Some typ ->
            parse_and_typecheck_string expr typ |> print_typecheck_result
        | Some _, None ->
            Out_channel.output_string stderr
              "Error: please provide type using -t flag";
            Out_channel.newline stderr
        | None, Some _ ->
            Out_channel.output_string stderr
              "Error: please provide expression using -e flag";
            Out_channel.newline stderr
        | None, None ->
            Out_channel.output_string stderr
              "Error: please provide expression and type as CLI flags";
            Out_channel.newline stderr)

let infer =
  Command.basic ~summary:"Type inference"
    Command.Let_syntax.(
      let%map_open expr_opt =
        flag "-e" (optional string)
          ~doc:"string Infer the type of the given [expression]"
      and filename =
        anon (maybe_with_default "-" ("filename" %: Filename.arg_type))
      in
      fun () ->
        match expr_opt with
        | Some expr ->
            parse_and_typeinfer (String expr) |> print_typeinfer_result
        | None ->
            filename |> input_kind_of_filename |> parse_and_typeinfer
            |> print_typeinfer_result)

let eval =
  Command.basic ~summary:"Expression evaluation"
    Command.Let_syntax.(
      let%map_open expr_opt =
        flag "-e" (optional string) ~doc:"string Evaluate the given expression"
      and filename =
        anon (maybe_with_default "-" ("filename" %: Filename.arg_type))
      in
      fun () ->
        match expr_opt with
        | Some expr -> parse_and_eval (String expr) |> print_eval_result
        | None ->
            filename |> input_kind_of_filename |> parse_and_eval
            |> print_eval_result)

let command =
  Command.group ~summary:"Manipulate dates"
    [ ("parse", parse); ("check", check); ("infer", infer); ("eval", eval) ]

let () = Command.run command
