open Base

type t

type 'a located = { data : 'a; loc : t } [@@deriving equal, sexp]

val locate : 'a -> Lexing.position -> Lexing.position -> 'a located

val mkLocated : ?loc: t -> 'a -> 'a located

val pp : ?msg:string -> t -> string
