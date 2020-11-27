open Base

type t = Z.t

let equal = Z.equal

let of_string = Z.of_string

let to_string = Z.to_string

let t_of_sexp se = Z.of_string (Sexp.to_string se)

let sexp_of_t z = Sexp.Atom (Z.to_string z)

let mul = Z.mul
