open Base
open Ast

type error = string

val check_open :
  Type.t Env.M.t -> Type.t Env.R.t -> Expr.t -> Type.t -> (unit, error) Result.t
(** [check_open delta gamma term type] typechecks a possibly open term [term]
    in modal (valid) typing context [delta] and regular typing context [gamma].
 *)

val check : Expr.t -> Type.t -> (unit, error) Result.t
(** Typecheck a closed term *)

val infer_open :
  Type.t Env.M.t -> Type.t Env.R.t -> Expr.t -> (Type.t, error) Result.t
(** [infer_open delta gamma term] infers the type of a possibly open term [term]
    in modal (valid) typing context [delta] and regular typing context [gamma].
 *)

val infer : Expr.t -> (Type.t, error) Result.t
(** Infer the type of a closed term *)
