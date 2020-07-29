open Ast

(** Convert ASTs to PPrint's [documents] *)
module type DOC = sig

(** Pretty-print types *)
val of_type : Type.t -> PPrint.document

(** Pretty-print expressions *)
val of_expr : Expr.t -> PPrint.document

(** Pretty-print literals (performs substitutions of literals for closures) *)
val of_lit : Lit.t -> PPrint.document
end

module Doc : DOC
