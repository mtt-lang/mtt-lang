open Base
open Stdio
open Result.Let_syntax
open Mtt
open ParserInterface

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

(* Parsing with error handling utilities *)
let parse_from_e : type a. a ast_kind -> input_kind -> (a, error) Result.t =
 fun ast_kind source ->
  parse_from ast_kind source
  |> Result.map_error ~f:(fun parse_error ->
         [%string "Parse error: $parse_error"])

(* Parsing and typechecking with error handling utilities *)
let parse_and_typecheck source typ_str =
  let%bind ast = parse_from_e Term source in
  let%bind typ = parse_from_e Type (String typ_str) in
  Typechecker.check ast typ
  |> Result.map_error ~f:(fun infer_err ->
         [%string "Typechecking error: $(format_located_error infer_err)"])

(* Parsing and type inference with error handling utilities *)
let parse_and_typeinfer source =
  let%bind ast = parse_from_e Term source in
  Typechecker.infer ast
  |> Result.map_error ~f:(fun infer_err ->
         [%string "Type inference error: $(format_located_error infer_err)"])

(* Parsing and evaluation with error handling utilities *)
let parse_and_eval source =
  let open Result.Let_syntax in
  let%bind ast = parse_from_e Term source in
  Evaluator.eval ast
  |> Result.map_error ~f:(fun eval_err ->
         [%string "Evaluation error: $(format_error eval_err)"])

let osource source_file source_arg =
  match (source_file, source_arg) with
  | Some filename, None -> Some (File filename)
  | None, Some string -> Some (String string)
  | Some _, Some _ -> None
  | None, None -> Some Stdin

let parse_expr source_file source_arg =
  match osource source_file source_arg with
  | None -> `Error (true, "Please provide exactly one expression to parse")
  | Some source -> (
      match parse_from_e Term source with
      | Ok ast ->
          let document = PrettyPrinter.Doc.of_expr ast in
          PPrint.ToChannel.pretty 1.0 80 stdout document;
          Out_channel.newline stdout;
          `Ok ()
      | Error err_msg -> `Error (false, err_msg) )

let check_expr source_file source_arg typ verbose =
  match osource source_file source_arg with
  | None -> `Error (true, "Please provide exactly one expression to typecheck")
  | Some source -> (
      match parse_and_typecheck source typ with
      | Ok () ->
          if verbose then (
            Out_channel.output_string stdout "OK. Expression typechecks.";
            Out_channel.newline stdout );
          `Ok ()
      | Error err_msg -> `Error (false, err_msg) )

let infer_type source_file source_arg =
  match osource source_file source_arg with
  | None ->
      `Error (true, "Please provide exactly one expression to infer its type")
  | Some source -> (
      match parse_and_typeinfer source with
      | Ok typ ->
          let document = PrettyPrinter.Doc.of_type typ in
          PPrint.ToChannel.pretty 1.0 80 stdout document;
          Out_channel.newline stdout;
          `Ok ()
      | Error err_msg -> `Error (false, err_msg) )

let eval_expr source_file source_arg =
  match osource source_file source_arg with
  | None -> `Error (true, "Please provide exactly one expression to evaluate")
  | Some source -> (
      match parse_and_eval source with
      | Ok value ->
          let document = PrettyPrinter.Doc.of_val value in
          PPrint.ToChannel.pretty 1.0 80 stdout document;
          Out_channel.newline stdout;
          `Ok ()
      | Error err_msg -> `Error (false, err_msg) )

(* Command line interface *)

open Cmdliner

let help man_format cmds topic =
  match topic with
  | None -> `Help (`Pager, None)
  | Some topic -> (
      let topics = "topics" :: "patterns" :: "environment" :: cmds in
      let conv, _ =
        Cmdliner.Arg.enum (List.rev_map ~f:(fun s -> (s, s)) topics)
      in
      match conv topic with
      | `Error e -> `Error (false, e)
      | `Ok t when String.( = ) t "topics" ->
          List.iter topics ~f:print_endline;
          `Ok ()
      | `Ok t when List.mem cmds t ~equal:String.( = ) ->
          `Help (man_format, Some t)
      | `Ok _t ->
          let page =
            ((topic, 7, "", "", ""), [ `S topic; `P "Say something" ])
          in
          `Ok (Cmdliner.Manpage.print man_format Caml.Format.std_formatter page)
      )

let help_secs =
  [
    `S Manpage.s_common_options;
    `P "These options are common to all commands.";
    `S "MORE HELP";
    `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
    `Noblank;
    `S Manpage.s_bugs;
    `P
      "Check bug reports at \
       https://github.com/anton-trunov/modal-type-theory/issues/";
  ]

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about mtt and mtt commands" in
  let man =
    [
      `S Manpage.s_description;
      `P "Prints help about mtt commands.";
      `Blocks help_secs;
    ]
  in
  ( Term.(ret (const help $ Arg.man_format $ Term.choice_names $ topic)),
    Term.info "help" ~doc ~exits:Term.default_exits ~man )

let parse_cmd =
  let source_file =
    let doc = "The file with expression to parse and pretty-print." in
    Arg.(value & pos 0 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)
  in
  let source_arg =
    let doc =
      "The expression to parse and pretty-print given as a CLI argument."
    in
    Arg.(
      value
      & opt (some string) None
      & info [ "e"; "expression" ] ~docv:"EXPRESSION" ~doc)
  in
  let doc = "parse and pretty-print an expression" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Parses and pretty-prints an expression either from stdin, a file, or \
         given as a command-line parameter";
      `Blocks help_secs;
    ]
  in
  ( Term.(ret (const parse_expr $ source_file $ source_arg)),
    Term.info "parse" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )

let check_cmd =
  let type_arg =
    let doc = "The expected type given as a CLI argument." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"TYPE" ~doc)
  in
  let source_file =
    let doc = "The file with expression to typecheck." in
    Arg.(value & pos 1 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)
  in
  let source_arg =
    let doc = "The expression to typecheck given as a CLI argument." in
    Arg.(
      value
      & opt (some string) None
      & info [ "e"; "expression" ] ~docv:"EXPRESSION" ~doc)
  in
  let verbose_arg =
    let doc = "Verbose mode." in
    Arg.(value & flag & info [ "verbose" ] ~doc)
  in
  let doc = "typecheck an expression" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Typecheck an expression either from stdin, a file, or given as a \
         command-line parameter";
      `Blocks help_secs;
    ]
  in
  ( Term.(
      ret (const check_expr $ source_file $ source_arg $ type_arg $ verbose_arg)),
    Term.info "check" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )

let infer_cmd =
  let source_file =
    let doc = "The file with expression to infer its type." in
    Arg.(value & pos 0 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)
  in
  let source_arg =
    let doc = "The expression to infer the type of given as a CLI argument." in
    Arg.(
      value
      & opt (some string) None
      & info [ "e"; "expression" ] ~docv:"EXPRESSION" ~doc)
  in
  let doc = "infer the type of an expression" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Infers the type of an expression either from stdin, a file, or given \
         as a command-line parameter";
      `Blocks help_secs;
    ]
  in
  ( Term.(ret (const infer_type $ source_file $ source_arg)),
    Term.info "infer" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )

let eval_cmd =
  let source_file =
    let doc = "The file with expression to evaluate." in
    Arg.(value & pos 0 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)
  in
  let source_arg =
    let doc = "The expression to evaluate given as a CLI argument." in
    Arg.(
      value
      & opt (some string) None
      & info [ "e"; "expression" ] ~docv:"EXPRESSION" ~doc)
  in
  let doc = "evaluate an expression" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Evaluates an expression either from stdin, a file, or given as a \
         command-line parameter";
      `Blocks help_secs;
    ]
  in
  ( Term.(ret (const eval_expr $ source_file $ source_arg)),
    Term.info "eval" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )

let default_cmd =
  let doc = "a modal type theory implementation" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  let man = help_secs in
  ( Term.(ret (const (`Help (`Pager, None)))),
    Term.info "mtt" ~version:"v0.0.0" ~doc ~sdocs ~exits ~man )

let cmds = [ parse_cmd; check_cmd; infer_cmd; eval_cmd; help_cmd ]

let () = Term.(exit @@ eval_choice default_cmd cmds)
