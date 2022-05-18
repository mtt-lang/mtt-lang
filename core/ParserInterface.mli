open Base
open Ast

type error = string
type filename = string

type _ ast_kind =
  | Type : Type.t ast_kind
  | Term : Expr.t ast_kind
  | Prog : Program.t ast_kind

type input_kind = Stdin | String of string | File of filename

val parse_from : 'a ast_kind -> input_kind -> ('a, error) Result.t
val parse_from_stdin : 'a ast_kind -> ('a, error) Result.t
val parse_from_string : 'a ast_kind -> string -> ('a, error) Result.t
val parse_from_file : 'a ast_kind -> filename -> ('a, error) Result.t
