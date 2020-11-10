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

  let let_kwd = !^"let"

  let letbox_kwd = !^"letbox"

  let in_kwd = !^"in"

  let rec of_type = function
    | Location.{ data = Type.Unit; _ } -> unit_type
    | Location.{ data = Type.Base idT; _ } -> !^idT
    | Location.{ data = Type.Prod (t1, t2); _ } ->
        parens (of_type t1 ^^ cross ^^ of_type t2)
    | Location.{ data = Type.Arr (dom, cod); _ } ->
        parens (of_type dom ^^^ arrow ^^^ of_type cod)
    | Location.{ data = Type.Box t; _ } -> box_type ^^ of_type t

  (** Pretty-print expressions with free vars substituited with
    their corresponding values from a regular environment *)
  let rec of_expr_with_free_vars_r bound_vars lenv expr =
    let open Expr in
    let rec walk bvs = function
      | Location.{ data = Unit; _ } -> unit_term
      | Location.{ data = Pair (e1, e2); _ } ->
          angles (walk bvs e1 ^^ comma ^/^ walk bvs e2)
      | Location.{ data = Fst pe; _ } -> group (parens (fst_kwd ^^ walk bvs pe))
      | Location.{ data = Snd pe; _ } -> group (parens (snd_kwd ^^ walk bvs pe))
      | Location.{ data = VarL idl; _ } -> (
          if
            (* To print free regular variables we use a regular environment with literals *)
            Set.mem bvs idl
          then !^(Id.R.to_string idl)
          else
            match Env.lookup_r lenv idl with
            | Ok literal -> parens (of_lit literal)
            | Error _msg ->
                failwith
                  "The precondition for calling Doc.of_expr_with_free_vars_r \
                   function is violated" )
      | Location.{ data = VarG idg; _ } -> !^(Id.M.to_string idg)
      | Location.{ data = Fun (idl, t_of_id, body); _ } ->
          parens
            ( fun_kwd
            ^^ !^(Id.R.to_string idl)
            ^^ colon ^^ of_type t_of_id ^^ dot ^^ space
            ^^ walk (Set.add bvs idl) body )
      | Location.{ data = App (fe, arge); _ } ->
          group (parens (walk bvs fe ^/^ walk bvs arge))
      | Location.{ data = Box e; _ } ->
          group (parens (box_kwd ^^ space ^^ walk bvs e))
      | Location.{ data = Let (idr, bound_e, body); _ } ->
          parens
            (group
               ( let_kwd
               ^^^ !^(Id.R.to_string idr)
               ^^^ equals ^^^ walk bvs bound_e ^^^ in_kwd
               ^/^ walk (Set.add bvs idr) body ))
      | Location.{ data = Letbox (idg, boxed_e, body); _ } ->
          parens
            (group
               ( letbox_kwd
               ^^^ !^(Id.M.to_string idg)
               ^^^ equals ^^^ walk bvs boxed_e ^^^ in_kwd ^/^ walk bvs body ))
    in
    walk bound_vars expr

  (* This prints an expression as-is, i.e. no substitutions for free vars *)
  and of_expr e = of_expr_with_free_vars_r (Set.empty (module Id.R)) Env.emp_r e

  and of_lit = function
    | Location.{ data = Val.Unit; _ } -> unit_term
    | Location.{ data = Val.Pair (l1, l2); _ } ->
        group (angles (of_lit l1 ^^ comma ^/^ of_lit l2))
    | Location.{ data = Val.Clos (idl, body, lenv); _ } ->
        fun_kwd
        ^^ !^(Id.R.to_string idl)
        ^^ dot
        ^^^
        (* when print out closures, substitute the free vars in its body with
           the corresponding literals from the closures' regular environment *)
        let bound_vars = Set.singleton (module Id.R) idl in
        of_expr_with_free_vars_r bound_vars lenv body
    | Location.{ data = Val.Box e; _ } -> box_kwd ^^^ of_expr e
end
