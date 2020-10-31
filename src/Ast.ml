open Base

type idT = string [@@deriving equal, sexp]

type 'ast astAndPosition = 
  {
    ast : 'ast; 
    start_pos : (Lexing.position sexp_opaque) [@equal.ignore];
    end_pos : (Lexing.position sexp_opaque) [@equal.ignore];
  }
  [@@deriving equal, sexp]

(** Types *)
module Type = struct
  type ast =
    | Unit  (** Unit type *)
    | Base of idT astAndPosition
        (** Base uninterpreted types, meaning there are no canonical terms inhabiting these types *)
    | Prod of (ast * ast) astAndPosition (** Type of pairs *)
    | Arr of (ast * ast) astAndPosition  (** Type of functions *)
    | Box of ast astAndPosition (** Type-level box *)
    [@@deriving equal, sexp]

  type t = ast astAndPosition [@@deriving equal, sexp]

  let mkType ast start_pos end_pos = {ast; start_pos; end_pos; }
end

(** Expressions *)
module Expr = struct
  type t =
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
    | Letbox of Id.M.t * t * t  (** [letbox u = expr1 in expr2] *)
  [@@deriving sexp]
end

(** Values *)
module Val = struct
  type t =
    | Unit  (** [unit] literal *)
    | Pair of t * t  (** [(lit1, lit2)] -- a pair of literals is a literal *)
    | Clos of Id.R.t * Expr.t * t Env.l  (** Deeply embedded closures *)
    | Box of Expr.t
        (** [box] literal, basically it's an unevaluated expression *)
  [@@deriving sexp]
end
