open Base
open Ast

type error =
  [ `TypeMismatchError of string
  | `UnboundRegularVarInsideBoxError of Location.t * string
    (** var location, message *)
  | Env.error ]

type 'e lerror = ([> error ] as 'e) Location.located

val check_open :
  Type.t Env.M.t ->
  Type.t Env.R.t ->
  Expr.t ->
  Type.t ->
  (unit, 'e lerror) Result.t
(** [check_open delta gamma term type] typechecks a possibly open term [term]
    in modal (valid) typing context [delta] and regular typing context [gamma].
 *)

val check : Expr.t -> Type.t -> (unit, 'e lerror) Result.t
(** Typecheck a closed term *)

val infer_open :
  Type.t Env.M.t -> Type.t Env.R.t -> Expr.t -> (Type.t, 'e lerror) Result.t
(** [infer_open delta gamma term] infers the type of a possibly open term [term]
    in modal (valid) typing context [delta] and regular typing context [gamma].
 *)

val infer : Expr.t -> (Type.t, 'e lerror) Result.t
(** Infer the type of a closed term *)
