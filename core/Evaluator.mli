open Base
open Ast

type error = [ `EvaluationError of string | Env.error ]
(** Type of errors *)

val free_vars_m : Expr.t -> (Id.M.t, Id.M.comparator_witness) Set.t
(** Compute the set of free modal variables in a term *)

val refresh_m :
  Id.M.t -> (Id.M.t, Id.M.comparator_witness) Set.t -> Id.M.t option
(** Given a modal identifier and a set of free modal variables,
    come up with a new name that won't capture any of the free modal variables.
    Returns [None] if no refreshment is needed and [Some new_name] otherwise *)

val subst_m : Expr.t -> Id.M.t -> Expr.t -> Expr.t
(** Capture-avoiding modal substitution: "[term/idm]body",
    i.e. substitute [term] for free variable [idm] in [body] *)

val eval_expr_open : Val.t Env.R.t -> Expr.t -> (Val.t, [> error ]) Result.t
(** Evaluate a possibly open term in a regular context *)

val eval_prog_open : Val.t Env.R.t -> Program.t -> (Val.t, [> error ]) Result.t
(** Evaluate a program in a regular context *)

val eval : Program.t -> (Val.t, [> error ]) Result.t
(** Evaluate a well-formed program *)
