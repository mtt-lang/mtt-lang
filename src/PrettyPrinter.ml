open Base
open PPrint
open PPrintCombinators
open Ast

module type DOC = sig
val of_type : Type.t -> PPrint.document
val of_expr : Expr.t -> PPrint.document
val of_lit : Lit.t -> PPrint.document
end


(** Convert ASTs to [doc] *)
module Doc : DOC = struct

(** concat two documents with an unbreakable space *)
let (^^^) left right = left ^^ space ^^ right

let cross = !^"×"
let unit_type = !^"()"
let unit_term = !^"()"
let arrow = !^"→"
let box_type = !^"□"

(* keywords *)
let fst_kwd = !^"π₁"
let snd_kwd = !^"π₂"
let box_kwd = !^"box"
let fun_kwd = !^"λ"
let letbox_kwd = !^"letbox"
let in_kwd = !^"in"


let rec of_type = function
  | Type.Unit -> unit_type
  | Type.Base idT -> !^idT
  | Type.Prod (t1, t2) -> parens (of_type t1 ^^ cross ^^ of_type t2)
  | Type.Arr (dom, cod) -> parens (of_type dom ^^^ arrow ^^^ of_type cod)
  | Type.Box t -> box_type ^^ (of_type t)

(** Pretty-print expressions with free vars substituited with
    their corresponding values from a local environment *)
let rec of_expr_with_free_vars_l bound_vars lenv expr =
  let open Expr in
  let rec walk bvs = function
    | Unit -> unit_term
    | Pair (e1, e2) -> angles (walk bvs e1 ^^ comma ^/^ walk bvs e2)
    | Fst pe -> group (parens (fst_kwd ^^ walk bvs pe))
    | Snd pe -> group (parens (snd_kwd ^^ walk bvs pe))
    | VarL idl ->
        (* To print free local variables we use a local environment with literals *)
        if Set.mem bvs idl then !^(Id.L.to_string idl)
        else begin match Env.lookup_l lenv idl with
             | Ok literal -> parens (of_lit literal)
             | Error _msg -> failwith "The precondition for calling Doc.of_expr_with_free_vars_l function is violated"
             end
    | VarG idg -> !^(Id.G.to_string idg)
    | Fun (idl, t_of_id, body) ->
        parens (fun_kwd ^^ !^(Id.L.to_string idl) ^^ colon ^^ (of_type t_of_id) ^^ dot ^^ space ^^
          walk (Set.add bvs idl) body)
    | App (fe, arge) -> group (parens (walk bvs fe ^/^ walk bvs arge))
    | Box e -> group (parens (box_kwd ^^ space ^^ walk bvs e))
    | Letbox (idg, boxed_e, body) ->
        parens (group (letbox_kwd ^^^ !^(Id.G.to_string idg) ^^^ equals ^^^
          walk bvs boxed_e ^^^ in_kwd ^/^
          walk bvs body))
  in walk bound_vars expr

(* This prints an expression as-is, i.e. no substitutions for free vars *)
and of_expr e = of_expr_with_free_vars_l (Set.empty (module Id.L)) Env.emp_l e

and of_lit = function
  | Lit.Unit -> unit_term
  | Lit.Pair (l1, l2) -> group (angles (of_lit l1 ^^ comma ^/^ of_lit l2))
  | Lit.Clos (idl, body, lenv) ->
      fun_kwd ^^ !^(Id.L.to_string idl) ^^ dot ^^^
      (* when print out closures, substitute the free vars in its body with
         the corresponding literals from the closures' local environment *)
      let bound_vars = Set.singleton (module Id.L) idl in
      of_expr_with_free_vars_l bound_vars lenv body
  | Lit.Box e -> of_expr e

end
