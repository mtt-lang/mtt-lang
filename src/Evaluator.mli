open Base
open Ast

(** Type of errors *)
type error = string

(** Compute the set of free modal variables in a term *)
val free_vars_g : Expr.t -> (Id.M.t, Id.M.comparator_witness) Set.t

(** Given a modal identifier and a set of free modal variables,
    come up with a new name that won't capture any of the free modal variables.
    Returns [None] if no refreshment is needed and [Some new_name] otherwise *)
val refresh_g : Id.M.t -> (Id.M.t, Id.M.comparator_witness) Set.t -> Id.M.t option

(** Capture-avoiding modal substitution: "[term/idg]body",
    i.e. substitute [term] for free variable [idg] in [body] *)
val subst_m : Expr.t -> Id.M.t -> Expr.t -> Expr.t

(** Evaluate a possibly open term in a regular context *)
val eval_open : Val.t Env.l -> Expr.t -> (Val.t, error) Result.t

(** Evaluate a closed term *)
val eval : Expr.t -> (Val.t, error) Result.t
