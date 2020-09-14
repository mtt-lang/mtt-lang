open Base
open Ast

(** Type of errors *)
type error = string

(** Compute the set of free global variables in a term *)
val free_vars_g : Expr.t -> (Id.G.t, Id.G.comparator_witness) Set.t

(** Given a global identifier and a set of free global variables,
    come up with a new name that won't capture any of the free global variables.
    Returns [None] if no refreshment is needed and [Some new_name] otherwise *)
val refresh_g : Id.G.t -> (Id.G.t, Id.G.comparator_witness) Set.t -> Id.G.t option

(** Capture-avoiding modal substitution: "[term/idg]body",
    i.e. substitute [term] for free variable [idg] in [body] *)
val subst_m : Expr.t -> Id.G.t -> Expr.t -> Expr.t

(** Evaluate a possibly open term in a local context *)
val eval_open : Val.t Env.l -> Expr.t -> (Val.t, error) Result.t

(** Evaluate a closed term *)
val eval : Expr.t -> (Val.t, error) Result.t
