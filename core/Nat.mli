open Base

type t [@@deriving equal, sexp]

include Stringable.S with type t := t

val zero : t
val one : t
val of_int : int -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val pred : t -> t
