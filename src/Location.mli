open Base

type t

type 'a located = { data : 'a; loc : t } [@@deriving equal, sexp]

val locate_start_end : 'a -> Lexing.position -> Lexing.position -> 'a located

val mk_located : ?loc:t -> 'a -> 'a located

val pp : ?msg:string -> t -> string
