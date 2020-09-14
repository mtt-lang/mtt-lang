open Base

(** Environment interface *)

(** Modal environment (\Delta, or environment for validities).
    Parameterized by the type of values it stores *)
type 'a g

(** Regular environment (\Gamma).
    Parameterized by the type of values it stores *)
type 'a l [@@deriving sexp]

(** Constructors *)

(** Empty modal environment *)
val emp_g : 'a g

(** Extend modal environment with an id and the corresponding value *)
val extend_g : 'a g -> Id.M.t -> 'a -> 'a g

(** Empty regular environment *)
val emp_l : 'a l

(** Extend regular environment with an id and the corresponding value *)
val extend_l : 'a l -> Id.R.t -> 'a -> 'a l


(** Destructors *)

(** Find the value corresponding to a modal identifier *)
val lookup_g : 'a g -> Id.M.t -> ('a, string) Result.t

(** Find the value corresponding to a regular identifier *)
val lookup_l : 'a l -> Id.R.t -> ('a, string) Result.t

