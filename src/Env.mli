open Base

(** Environment interface *)

(** Global environment (\Delta, or environment for validities).
    Parameterized by the type of values it stores *)
type 'a g

(** Local environment (\Gamma).
    Parameterized by the type of values it stores *)
type 'a l [@@deriving sexp]

(** Constructors *)

(** Empty global environment *)
val emp_g : 'a g

(** Extend global environment with an id and the corresponding value *)
val extend_g : 'a g -> Id.G.t -> 'a -> 'a g

(** Empty local environment *)
val emp_l : 'a l

(** Extend local environment with an id and the corresponding value *)
val extend_l : 'a l -> Id.L.t -> 'a -> 'a l


(** Destructors *)

(** Find the value corresponding to a global identifier *)
val lookup_g : 'a g -> Id.G.t -> ('a, string) Result.t

(** Find the value corresponding to a local identifier *)
val lookup_l : 'a l -> Id.L.t -> ('a, string) Result.t

