open Base

module type ID = sig
(** Interface for identifiers *)
type t [@@deriving equal, sexp]

val mk : string -> t
val to_string : t -> string

include Comparable.S with type t := t
end

(** Local context (\Gamma) identifiers *)
module L : ID = struct
  module T = struct
    type t = string [@@deriving equal, compare, sexp]
    let mk = Fn.id
    let to_string = Fn.id
  end
  include T
  include Comparable.Make(T)
end

(** Global context (\Delta) identifiers *)
module G : ID = struct
  module T = struct
    type t = string [@@deriving equal, compare, sexp]
    let mk = Fn.id
    let to_string = Fn.id
  end
  include T
  include Comparable.Make(T)
end
