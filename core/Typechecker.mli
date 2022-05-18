open Base
open Ast

type error =
  [ `TypeMismatchError of string
  | `UnboundRegularVarInsideBoxError of Location.t * string
    (** var location, message *)
  | Env.error ]

type 'e lerror = ([> error ] as 'e) Location.located

val check_expr_open :
  Type.t Env.M.t ->
  Type.t Env.R.t ->
  Expr.t ->
  Type.t ->
  (unit, 'e lerror) Result.t
(** [check_open delta gamma term type] typechecks a possibly open term [term]
    in modal (valid) typing context [delta] and regular typing context [gamma].
 *)

val check_prog_open :
  Type.t Env.R.t -> Program.t -> Type.t -> (unit, 'e lerror) Result.t

val check : Program.t -> Type.t -> (unit, 'e lerror) Result.t
(** Typecheck a closed term *)

val infer_expr_open :
  Type.t Env.M.t -> Type.t Env.R.t -> Expr.t -> (Type.t, 'e lerror) Result.t
(** [infer_open delta gamma term] infers the type of a possibly open term [term]
    in modal (valid) typing context [delta] and regular typing context [gamma].
 *)

val infer_prog_open :
  Type.t Env.R.t -> Program.t -> (Type.t, 'e lerror) Result.t

val infer : Program.t -> (Type.t, 'e lerror) Result.t
(** Infer the type of a closed term *)
