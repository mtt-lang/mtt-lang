open Base
open Ast

type error = string
(** Type of errors *)

val free_vars_m : Expr.t -> (Id.M.t, Id.M.comparator_witness) Set.t
(** Compute the set of free modal variables in a term *)

val refresh_m :
  Id.M.t -> (Id.M.t, Id.M.comparator_witness) Set.t -> Id.M.t option
(** Given a modal identifier and a set of free modal variables,
    come up with a new name that won't capture any of the free modal variables.
    Returns [None] if no refreshment is needed and [Some new_name] otherwise *)

val refresh_r :
  Id.R.t -> (Id.R.t, Id.R.comparator_witness) Set.t -> Id.R.t option
(** Given a regular identifier and a set of free regular variables,
    come up with a new name that won't capture any of the free regular variables.
    Returns [None] if no refreshment is needed and [Some new_name] otherwise *)

val subst_m : Expr.t -> Id.M.t -> Expr.t -> Expr.t
(** Capture-avoiding modal substitution: "[term/idg]body",
    i.e. substitute [term] for free variable [idg] in [body] *)

val eval_open : Val.t Env.r -> Expr.t -> (Val.t, error) Result.t
(** Evaluate a possibly open term in a regular context *)

val eval : Expr.t -> (Val.t, error) Result.t
(** Evaluate a closed term *)
