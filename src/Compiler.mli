open Ast

val compile : Expr.t -> Malfunction.t

val mval2val : Malfunction_interpreter.value -> Val.t
(** this is temporary function for working 
with malfunction interpreter *)
