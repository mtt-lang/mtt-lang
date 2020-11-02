open Base

(** Environment interface *)

type 'a m
(** Modal environment (\Delta, or environment for validities).
    Parameterized by the type of values it stores *)

type 'a r [@@deriving sexp]
(** Regular environment (\Gamma).
    Parameterized by the type of values it stores *)

(** Constructors *)

val emp_m : 'a m
(** Empty modal environment *)

val extend_m : 'a m -> Id.M.t -> 'a -> 'a m
(** Extend modal environment with an id and the corresponding value *)

val emp_r : 'a r
(** Empty regular environment *)

val extend_r : 'a r -> Id.R.t -> 'a -> 'a r
(** Extend regular environment with an id and the corresponding value *)

(** Destructors *)

val lookup_m : 'a m -> Id.M.t -> ('a, string) Result.t
(** Find the value corresponding to a modal identifier *)

val lookup_r : 'a r -> Id.R.t -> ('a, string) Result.t
(** Find the value corresponding to a regular identifier *)
