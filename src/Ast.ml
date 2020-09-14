open Base

type idT = string [@@deriving equal, sexp]

(** Types *)
module Type = struct
type t =
  | Unit
    (** Unit type *)
  | Base of idT
    (** Base uninterpreted types, meaning there are no canonical terms inhabiting these types *)
  | Prod of t * t
    (** Type of pairs *)
  | Arr of t * t
    (** Type of functions *)
  | Box of t
    (** Type-level box *)
[@@deriving equal, sexp]
end


(** Expressions *)
module Expr = struct
type t =
  | Unit
    (** [unit] *)
  | Pair of t * t
    (** pairs [(expr1, expr2)] *)
  | Fst of t
    (** first projection of a pair *)
  | Snd of t
    (** second projection of a pair *)
  | VarL of Id.L.t
    (** variables of the local context *)
  | VarG of Id.G.t
    (** variables of the global context (or "valid variables"),
        these are syntactically distinct from the local (ordinary) variables *)
  | Fun of Id.L.t * Type.t * t
    (** anonymous functions: [fun (x : T) => expr] *)
  | App of t * t
    (** function application: [f x] *)
  | Box of t
    (** term-level box: [box expr1] *)
  | Letbox of Id.G.t * t * t
    (** [letbox u = expr1 in expr2] *)
[@@deriving sexp]
end

(** Values *)
module Val = struct
type t =
  | Unit
    (** [unit] literal *)
  | Pair of t * t
    (** [(lit1, lit2)] -- a pair of literals is a literal *)
  | Clos of Id.L.t * Expr.t * t Env.l
    (** Deeply embedded closures *)
  | Box of Expr.t
    (** [box] literal, basically it's an unevaluated expression *)
[@@deriving sexp]
end


