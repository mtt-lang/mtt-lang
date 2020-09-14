open Base

module type ID = sig
  type t [@@deriving equal, sexp]
  (** Interface for identifiers *)

  val mk : string -> t

  val to_string : t -> string

  include Comparable.S with type t := t
end

(** Regular context (\Gamma) identifiers *)
module R : ID = struct
  module T = struct
    type t = string [@@deriving equal, compare, sexp]

    let mk = Fn.id

    let to_string = Fn.id
  end

  include T
  include Comparable.Make (T)
end

(** Modal context (\Delta) identifiers *)
module M : ID = struct
  module T = struct
    type t = string [@@deriving equal, compare, sexp]

    let mk = Fn.id

    let to_string = Fn.id
  end

  include T
  include Comparable.Make (T)
end
