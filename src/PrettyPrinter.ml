open Base
open PPrint
open PPrintCombinators
open Ast

module type DOC = sig
  val of_type : Type.t -> PPrint.document

  val of_expr : Expr.t -> PPrint.document

  val of_lit : Val.t -> PPrint.document
end

(** Convert ASTs to [doc] *)
module Doc : DOC = struct
  (** concat two documents with an unbreakable space *)
  let ( ^^^ ) left right = left ^^ space ^^ right

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

  let rec of_type ty =
    match ty.Location.data with
    | Type.Unit -> unit_type
    | Type.Base idT -> !^idT
    | Type.Prod (t1, t2) -> parens (of_type t1 ^^ cross ^^ of_type t2)
    | Type.Arr (dom, cod) -> parens (of_type dom ^^^ arrow ^^^ of_type cod)
    | Type.Box t -> box_type ^^ of_type t

  (** Pretty-print expressions with free vars substituited with
    their corresponding values from a regular environment *)
  let rec of_expr_with_free_vars_l bound_vars lenv expr =
    let open Expr in
    let rec walk bvs term =
      match term.Location.data with
      | Unit -> unit_term
      | Pair (e1, e2) -> angles (walk bvs e1 ^^ comma ^/^ walk bvs e2)
      | Fst pe -> group (parens (fst_kwd ^^ walk bvs pe))
      | Snd pe -> group (parens (snd_kwd ^^ walk bvs pe))
      | VarL idl -> (
          if
            (* To print free regular variables we use a regular environment with literals *)
            Set.mem bvs idl
          then !^(Id.R.to_string idl)
          else
            match Env.lookup_l lenv idl with
            | Ok literal -> parens (of_lit literal)
            | Error _msg ->
                failwith
                  "The precondition for calling Doc.of_expr_with_free_vars_l \
                   function is violated" )
      | VarG idg -> !^(Id.M.to_string idg)
      | Fun (idl, t_of_id, body) ->
          parens
            ( fun_kwd
            ^^ !^(Id.R.to_string idl)
            ^^ colon ^^ of_type t_of_id ^^ dot ^^ space
            ^^ walk (Set.add bvs idl) body )
      | App (fe, arge) -> group (parens (walk bvs fe ^/^ walk bvs arge))
      | Box e -> group (parens (box_kwd ^^ space ^^ walk bvs e))
      | Letbox (idg, boxed_e, body) ->
          parens
            (group
               ( letbox_kwd
               ^^^ !^(Id.M.to_string idg)
               ^^^ equals ^^^ walk bvs boxed_e ^^^ in_kwd ^/^ walk bvs body ))
    in
    walk bound_vars expr

  (* This prints an expression as-is, i.e. no substitutions for free vars *)
  and of_expr e = of_expr_with_free_vars_l (Set.empty (module Id.R)) Env.emp_l e

  and of_lit lit =
    match lit.Location.data with
    | Val.Unit -> unit_term
    | Val.Pair (l1, l2) -> group (angles (of_lit l1 ^^ comma ^/^ of_lit l2))
    | Val.Clos (idl, body, lenv) ->
        fun_kwd
        ^^ !^(Id.R.to_string idl)
        ^^ dot
        ^^^
        (* when print out closures, substitute the free vars in its body with
           the corresponding literals from the closures' regular environment *)
        let bound_vars = Set.singleton (module Id.R) idl in
        of_expr_with_free_vars_l bound_vars lenv body
    | Val.Box e -> of_expr e
end
