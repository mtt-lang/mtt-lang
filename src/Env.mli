open Base

(** Environment interface *)

type 'a g
(** Modal environment (\Delta, or environment for validities).
    Parameterized by the type of values it stores *)

type 'a l [@@deriving sexp]
(** Regular environment (\Gamma).
    Parameterized by the type of values it stores *)

(** Constructors *)

val emp_g : 'a g
(** Empty modal environment *)

val extend_g : 'a g -> Id.M.t -> 'a -> 'a g
(** Extend modal environment with an id and the corresponding value *)

val emp_l : 'a l
(** Empty regular environment *)

val extend_l : 'a l -> Id.R.t -> 'a -> 'a l
(** Extend regular environment with an id and the corresponding value *)

(** Destructors *)

val lookup_g : 'a g -> Id.M.t -> ('a, string) Result.t
(** Find the value corresponding to a modal identifier *)

val lookup_l : 'a l -> Id.R.t -> ('a, string) Result.t
(** Find the value corresponding to a regular identifier *)
