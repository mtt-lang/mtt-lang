open Base

type t
type 'a located = { data : 'a; loc : t } [@@deriving equal, sexp]

val locate_start_end : 'a -> Lexing.position -> Lexing.position -> 'a located
val locate : ?loc:t -> 'a -> 'a located
val empty : t -> bool
val pp : ?msg:string -> t -> string
val pp_column_range : t -> string
