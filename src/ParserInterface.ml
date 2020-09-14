open Base
open Result.Let_syntax
module MI = Parser.MenhirInterpreter
module MG = MenhirLib.General
open Ast

type error = string

type filename = string

type _ ast_kind = Type : Type.t ast_kind | Term : Expr.t ast_kind

type input_kind = Stdin | String of string | File of filename

let parser_driver : type a. a ast_kind -> Lexing.position -> a MI.checkpoint =
  function
  | Type -> Parser.Incremental.type_eof
  | Term -> Parser.Incremental.expr_eof

let state checkpoint : int =
  match Lazy.force (MI.stack checkpoint) with
  | MG.Nil -> 0
  | MG.Cons (Element (s, _, _, _), _) -> MI.number s

let handle_syntax_error _lexbuf checkpoint =
  let message =
    match ParserErrors.message (state checkpoint) with
    | exception Caml.Not_found ->
        "Unrecognized syntax error. Please report your example"
    | msg -> msg
  in
  Result.fail message

(* let rec loop next_token lexbuf (checkpoint : Ast.Expr.t MI.checkpoint) = *)
let rec loop next_token lexbuf (checkpoint : 'a MI.checkpoint) =
  match checkpoint with
  | MI.InputNeeded _env ->
      let token = next_token () in
      let checkpoint = MI.offer checkpoint token in
      loop next_token lexbuf checkpoint
  | MI.Shifting _ | MI.AboutToReduce _ ->
      let checkpoint = MI.resume checkpoint in
      loop next_token lexbuf checkpoint
  | MI.HandlingError env -> handle_syntax_error lexbuf env
  | MI.Accepted ast -> return ast
  | MI.Rejected ->
      (* Cannot happen as we stop at syntax error immediatly *)
      assert false

let process parser_driver lexbuf =
  let lexer = Lexer.lexer lexbuf in
  try
    loop lexer lexbuf (parser_driver (fst @@ Sedlexing.lexing_positions lexbuf))
  with Lexer.LexError (_pos, _msg) ->
    (* It seems this cannot happen *)
    assert false

let parse_from_stdin ast_kind =
  let lexbuf = Sedlexing.Utf8.from_channel Stdio.stdin in
  process (parser_driver ast_kind) lexbuf

let parse_from_string ast_kind str =
  let lexbuf = Sedlexing.Utf8.from_string str in
  process (parser_driver ast_kind) lexbuf

let parse_from_file ast_kind filename =
  Stdio.In_channel.with_file filename ~f:(fun inchan ->
      let lexbuf = Sedlexing.Utf8.from_channel inchan in
      process (parser_driver ast_kind) lexbuf)

let parse_from ast_kind input_kind =
  match input_kind with
  | Stdin -> parse_from_stdin ast_kind
  | String str -> parse_from_string ast_kind str
  | File filename -> parse_from_file ast_kind filename
