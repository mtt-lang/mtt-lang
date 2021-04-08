open Base

type idT = string [@@deriving equal, sexp]

(** Types *)
module Type = struct
  type t =
    | Unit  (** Unit type *)
    | Nat  (** Type for numbers *)
    | Base of idT
        (** Base uninterpreted types, meaning there are no canonical terms inhabiting these types *)
    | Prod of { ty1 : t; ty2 : t }  (** Type of pairs *)
    | Arr of { dom : t; cod : t }  (** Type of functions *)
    | Box of { ty : t }  (** Type-level box *)
  [@@deriving equal, sexp]
end

(** Expressions *)
module Expr = struct
  (* binary arithmetic operations *)
  type binop = Add | Sub | Mul | Div [@@deriving equal, sexp]

  type t = t' Location.located

  and t' =
    | Unit  (** [unit] *)
    | Pair of t * t  (** pairs [(expr1, expr2)] *)
    | Fst of t  (** first projection of a pair *)
    | Snd of t  (** second projection of a pair *)
    | IntZ of Nat.t  (** numbers *)
    | BinOp of binop * t * t  (** binary arithmetic operations *)
    | VarL of Id.R.t  (** variables of the regular context *)
    | VarG of Id.M.t
        (** variables of the modal context (or "valid variables"),
        these are syntactically distinct from the regular (ordinary) variables *)
    | Fun of { idr : Id.R.t; ty_id : Type.t; body : t }
        (** anonymous functions: [fun (x : T) => expr] *)
    | Fix of Id.R.t * Type.t * Id.R.t * t 
        (** Fix combinator: fix f x = f (fix x) f *)
    | App of t * t  (** function application: [f x] *)
    | Box of t  (** term-level box: [box expr1] *)
    | Let of Id.R.t * t * t  (** [let u = expr1 in expr2] *)
    | Letbox of Id.M.t * t * t  (** [letbox u = expr1 in expr2] *)
  [@@deriving equal, sexp]

  (* Wrappers for constructors *)
  let unit = Location.locate Unit

  let pair e1 e2 = Location.locate @@ Pair { e1; e2 }

  let fst e = Location.locate @@ Fst { e }

  let snd e = Location.locate @@ Snd { e }

  let var_r idr = Location.locate @@ VarR { idr }

  let var_m idm = Location.locate @@ VarM { idm }

  let func idr ty_id body = Location.locate @@ Fun { idr; ty_id; body }

  let fix idl_fun t_of_id idl_body body = Location.locate @@ Fix (idl_fun, t_of_id, idl_body, body)

  let app fe arge = Location.locate @@ App (fe, arge)

  let box e = Location.locate @@ Box { e }

  let letc idr bound body = Location.locate @@ Let { idr; bound; body }

  let letbox idm boxed body = Location.locate @@ Letbox { idm; boxed; body }
end

(** Values *)
module Val = struct
  type t =
    | Unit  (** [unit] literal *)
    | IntZ of Nat.t
    | Pair of t * t  (** [(lit1, lit2)] -- a pair of literals is a literal *)
    | Clos of Id.R.t * Expr.t * t Env.r  (** Deeply embedded closures *)
    | ReClos of Id.R.t * Id.R.t * Expr.t * t Env.r  (** Recursion closures *)
    | Box of Expr.t
        (** [box] literal, basically it's an unevaluated expression *)
  [@@deriving sexp]
end
