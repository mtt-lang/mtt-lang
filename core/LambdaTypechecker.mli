open Ast

(* open Elpi.API.Ast *)

val linfer : Expr.t -> (Ast.Type.t, string) Result.t
