open Base
open Mtt

type error = [ `CompilationError of string | CamInterpreter.error | Env.error ]
(** type of errors *)

val compile : Ast.Expr.t -> (Cam.instructionCAM list, [> error ]) Result.t
(** compile mtt-expr to CAM bytecode  *)
