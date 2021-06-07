open Mtt

type error = [ `CompilationError of string | Env.error ]
(** type of errors *)

val compile : Ast.Expr.t -> (Cam.instructionCAM list, [> error ]) Result.t
