open Base

type idT = string [@@deriving equal, sexp]

(** Types *)
module Type = struct
  type t =
    | Unit  (** Unit type *)
    | Base of idT
        (** Base uninterpreted types, meaning there are no canonical terms inhabiting these types *)
    | Prod of t * t  (** Type of pairs *)
    | Arr of t * t  (** Type of functions *)
    | Box of t  (** Type-level box *)
  [@@deriving equal, sexp]
end

(** Expressions *)
module Expr = struct
  type t = t' Location.located

  and t' =
    | Unit  (** [unit] *)
    | Pair of t * t  (** pairs [(expr1, expr2)] *)
    | Fst of t  (** first projection of a pair *)
    | Snd of t  (** second projection of a pair *)
    | VarL of Id.R.t  (** variables of the regular context *)
    | VarG of Id.M.t
        (** variables of the modal context (or "valid variables"),
        these are syntactically distinct from the regular (ordinary) variables *)
    | Fun of Id.R.t * Type.t * t
        (** anonymous functions: [fun (x : T) => expr] *)
    | App of t * t  (** function application: [f x] *)
    | Box of t  (** term-level box: [box expr1] *)
    | Let of Id.R.t * t * t  (** [let u = expr1 in expr2] *)
    | Letbox of Id.M.t * t * t  (** [letbox u = expr1 in expr2] *)
  [@@deriving equal, sexp]

  (* Wrappers for constructors *)
  let unit = Location.locate Unit

  let pair e1 e2 = Location.locate @@ Pair (e1, e2)

  let fst pe = Location.locate @@ Fst pe

  let snd pe = Location.locate @@ Snd pe

  let varl idl = Location.locate @@ VarL idl

  let varg idg = Location.locate @@ VarG idg

  let func idl t_of_id body = Location.locate @@ Fun (idl, t_of_id, body)

  let app fe arge = Location.locate @@ App (fe, arge)

  let box e = Location.locate @@ Box e

  let letc idl bound_e body = Location.locate @@ Let (idl, bound_e, body)

  let letbox idg boxed_e body = Location.locate @@ Letbox (idg, boxed_e, body)
end

(** Values *)
module Val = struct
  type t =
    | Unit  (** [unit] literal *)
    | Pair of t * t  (** [(lit1, lit2)] -- a pair of literals is a literal *)
    | Clos of Id.R.t * Expr.t * t Env.R.t  (** Deeply embedded closures *)
    | Box of Expr.t
        (** [box] literal, basically it's an unevaluated expression *)
  [@@deriving sexp]
end
