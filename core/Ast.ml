open Base

(** Types *)
module Type = struct
  type t = t' Location.located

  and t' =
    | Unit  (** Unit type *)
    | Nat  (** Type for numbers *)
    | Base of { idt : Id.T.t }
        (** Base uninterpreted types, meaning there are no canonical terms inhabiting these types *)
    | Prod of { ty1 : t; ty2 : t }  (** Type of pairs *)
    | Arr of { dom : t; cod : t }  (** Type of functions *)
    | Box of { ty : t }  (** Type-level box *)
  [@@deriving equal, sexp]

  let unit = Location.locate Unit
  let nat = Location.locate Nat
  let base idt = Location.locate @@ Base { idt }
  let prod ty1 ty2 = Location.locate @@ Prod { ty1; ty2 }
  let arr dom cod = Location.locate @@ Arr { dom; cod }
  let box ty = Location.locate @@ Box { ty }
end

(* Data constructor *)
module DataCtor = struct
  type t = { idd : Id.D.t; fields : Type.t list } [@@deriving sexp]
end

(* Pattern for pattern-matching *)
module Pattern = struct
  type t = t' Location.located

  and t' =
    | Ignore
    | VarR of { idr : Id.R.t }
    | DCtor of { idd : Id.D.t; subs : t list }
    | Pair of { sub1 : t; sub2 : t }
    | Nat of { n : Nat.t }
    | Unit
  [@@deriving equal, sexp]

  let ignore = Location.locate Ignore
  let var_r idr = Location.locate @@ VarR { idr }
  let d_ctor idd subs = Location.locate @@ DCtor { idd; subs }
  let pair sub1 sub2 = Location.locate @@ Pair { sub1; sub2 }
end

(* Type declaration *)
module TypeDecl = struct
  type t = DataCtor.t list [@@deriving sexp]
end

(** Expressions *)
module Expr = struct
  (* binary arithmetic operations *)
  type binop = Add | Sub | Mul | Div [@@deriving equal, sexp]

  type t = t' Location.located

  and t' =
    | Unit  (** [unit] *)
    | Pair of { e1 : t; e2 : t }  (** pairs [(expr1, expr2)] *)
    | Fst of { e : t }  (** first projection of a pair *)
    | Snd of { e : t }  (** second projection of a pair *)
    | Nat of { n : Nat.t }  (** numbers *)
    | BinOp of { op : binop; e1 : t; e2 : t }
        (** binary arithmetic operations *)
    | VarR of { idr : Id.R.t }  (** variables of the regular context *)
    | VarM of { idm : Id.M.t }
        (** variables of the modal context (or "valid variables"),
        these are syntactically distinct from the regular (ordinary) variables *)
    | VarD of { idd : Id.D.t }
        (** using type identifier (starting with a capital letter) means calling data constructor **)
    | Fun of { idr : Id.R.t; ty_id : Type.t; body : t }
        (** anonymous functions: [fun (x : T) => expr] *)
    | Fix of {
        self : Id.R.t;
        ty_id : Type.t;
        idr : Id.R.t;
        idr_ty : Type.t;
        body : t;
      }  (** Fix combinator: fix f x = f (fix x) f *)
    | App of { fe : t; arge : t }  (** function application: [f x] *)
    | Box of { e : t }  (** term-level box: [box expr1] *)
    | Let of { idr : Id.R.t; bound : t; body : t }
        (** [let u = expr1 in expr2] *)
    | Letbox of { idm : Id.M.t; boxed : t; body : t }
        (** [letbox u = expr1 in expr2] *)
    | Match of { matched : t; branches : (Pattern.t * t) list }
        (** Pattern-matching of general form:
            [match matched with 
              | pattern => branch_body
              | ...
            end] *)
  [@@deriving equal, sexp]

  (* Wrappers for constructors *)
  let unit = Location.locate Unit
  let nat n = Location.locate @@ Nat { n }
  let pair e1 e2 = Location.locate @@ Pair { e1; e2 }
  let fst e = Location.locate @@ Fst { e }
  let snd e = Location.locate @@ Snd { e }
  let binop op e1 e2 = Location.locate @@ BinOp { op; e1; e2 }
  let var_r idr = Location.locate @@ VarR { idr }
  let var_m idm = Location.locate @@ VarM { idm }
  let var_d idd = Location.locate @@ VarD { idd }
  let func idr ty_id body = Location.locate @@ Fun { idr; ty_id; body }

  let fix self ty_id idr idr_ty body =
    Location.locate @@ Fix { self; ty_id; idr; idr_ty; body }

  let app fe arge = Location.locate @@ App { fe; arge }
  let box e = Location.locate @@ Box { e }
  let letc idr bound body = Location.locate @@ Let { idr; bound; body }
  let letbox idm boxed body = Location.locate @@ Letbox { idm; boxed; body }

  let match_with matched branches =
    Location.locate @@ Match { matched; branches }
end

(* Program is a sequence of top-level declarations which must end with an expression.
   Type and value of a program is determined by the expression. *)
module Program = struct
  type t = t' Location.located

  and t' =
    | Let of { idr : Id.R.t; bound : Expr.t; next : t }
    | Type of { idt : Id.T.t; decl : TypeDecl.t; next : t }
    | Last of Expr.t
  [@@deriving sexp]
end

(** Values *)
module Val = struct
  type t =
    | Unit  (** [unit] value *)
    | Nat of { n : Nat.t }  (** nat *)
    | Pair of { v1 : t; v2 : t }
        (** [(lit1, lit2)] -- a pair of values is a value *)
    | RecClos of { self : Id.R.t; idr : Id.R.t; body : Expr.t; env : t Env.R.t }
        (** Recursion closures *)
    | Box of { e : Expr.t }
        (** [box] value, basically it's an unevaluated expression *)
    | DCtor of { idd : Id.D.t; args : t list }  (** data constructor **)
  [@@deriving sexp]
end
