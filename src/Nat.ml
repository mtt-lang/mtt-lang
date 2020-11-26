open Base

module Nat = struct
  type t = string [@@deriving equal, compare, sexp]
  let mk = Fn.id
  let nat_of_string = Z.big_int_of_string
  let t_of_sexp = Fn.id
end
