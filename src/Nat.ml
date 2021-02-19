open Base

type t = Z.t

let zero = Z.zero

let one = Z.one

let equal = Z.equal

let of_string = Z.of_string

let to_string = Z.to_string

let t_of_sexp se = Z.of_string (Sexp.to_string se)

let sexp_of_t z = Sexp.Atom (Z.to_string z)

let add = Z.add

let sub = Z.sub

let mul = Z.mul

let div = Z.div

let pred n = if equal n zero then zero else sub n one
