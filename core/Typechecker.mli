open Base
open Ast

type error =
  [ `TypeMismatchError of string
  | `UnboundRegularVarInsideBoxError of Location.t * string
    (** var location, message *)
  | `DataCtorArgsQuantityMismatch of string
  | `TypeOfEmptyMatchCannotBeInferred of string
  | `IrrefutablePatternExpected of string
  | Env.error ]

type 'e lerror = ([> error ] as 'e) Location.located

module Envs : sig
  type t = {
    modal : Type.t Env.M.t;
    regular : Type.t Env.R.t;
    types : TypeDecl.t Env.T.t;
    d_ctors : Type.t Env.D.t;
  }

  val extend_regular : t -> Id.R.t -> Type.t -> t
  val extend_modal : t -> Id.M.t -> Type.t -> t
  val extend_types : t -> Id.T.t -> TypeDecl.t -> t
  val extend_d_ctors : t -> Id.D.t -> Type.t -> t
  val emp : t
end

val check_expr_open : Envs.t -> Expr.t -> Type.t -> (unit, 'e lerror) Result.t
(** [check_expr_open envs term type] typechecks a possibly open term [term]
    in contexts [envs].
 *)

val check_prog_open :
  Envs.t -> Program.t -> Type.t -> (unit, 'e lerror) Result.t
(** [check_prog_open envs prog type] typechecks a program [prog]
    in contexts [envs].
  *)

val check : Program.t -> Type.t -> (unit, 'e lerror) Result.t
(** Typecheck a well-formed program. *)

val infer_expr_open : Envs.t -> Expr.t -> (Type.t, 'e lerror) Result.t
(** [infer_expr_open envs term] infers the type of a possibly open term [term]
    in contexts [envs].
 *)

val infer_prog_open : Envs.t -> Program.t -> (Type.t, 'e lerror) Result.t
(** [infer_prog_open envs term] infers the type of a program [prog]
    in contexts [envs].
 *)

val infer : Program.t -> (Type.t, 'e lerror) Result.t
(** Infer the type of a well-formed program. *)
