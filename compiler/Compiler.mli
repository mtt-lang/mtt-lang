open Mtt.Ast

val compile_naive : Expr.t -> Malfunction.t

val compile : Expr.t -> Malfunction.t

val obj2val : Obj.t -> Val.t
(** VERY UNSAFE FOR NOW!!!
    This is unsafe function for transform
    Obj.t to mtt value. The result must be
    a nat or a pair  *)
