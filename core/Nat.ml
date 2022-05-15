open Base

type t = Z.t

let zero = Z.zero
let one = Z.one
let equal = Z.equal
let of_string = Z.of_string
let to_string = Z.to_string
let t_of_sexp se = Z.of_string (Sexp.to_string se)
let sexp_of_t z = Sexp.Atom (Z.to_string z)
let of_int = Z.of_int
let add = Z.add
let sub n m = if Z.geq m n then zero else Z.sub n m
let mul = Z.mul
let div = Z.div
let pred n = if equal n zero then zero else sub n one
