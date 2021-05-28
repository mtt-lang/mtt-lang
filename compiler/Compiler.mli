open Mtt.Ast

val compile : Expr.t -> Cam.instructionCAM list

val compile_simple : Expr.t -> Malfunction.t

val obj2val : Obj.t -> Val.t
(** VERY UNSAFE FOR NOW!!!
    This is unsafe function for transform
    Obj.t to mtt value. The result must be
    a nat or a pair  *)

val cam2val : Cam.valueCAM -> Val.t
