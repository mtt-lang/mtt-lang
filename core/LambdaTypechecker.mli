open Ast

val linfer : Expr.t -> (Ast.Type.t, string) Result.t
