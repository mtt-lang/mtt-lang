open Ast

(** Convert ASTs to PPrint's [documents] *)
module type DOC = sig
  val of_type : Type.t -> PPrint.document
  (** Pretty-print types *)

  val of_expr : Expr.t -> PPrint.document
  (** Pretty-print expressions *)

  val of_val : Val.t -> PPrint.document
  (** Pretty-print values (performs substitutions of values for closures) *)
end

module Doc : DOC
