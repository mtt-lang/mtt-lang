open Base 

type position = Lexing.position =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
  [@@deriving equal, sexp]

type t

type 'a location = { data : 'a ; loc : t } [@@deriving equal, sexp]

val mkLocByPosition : 'a -> position -> position -> 'a location
val mkLocation : 'a -> t -> 'a location

val showLocation : 'a location -> string
val errorMsg : string -> 'a location -> string
