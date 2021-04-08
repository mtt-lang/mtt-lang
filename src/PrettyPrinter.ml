open Base
open PPrint
open PPrintCombinators
open Ast

module type DOC = sig
  val of_type : Type.t -> PPrint.document

  val of_expr : Expr.t -> PPrint.document

  val of_val : Val.t -> PPrint.document
end

(** Convert ASTs to [doc] *)
module Doc : DOC = struct
  (** concat two documents with an unbreakable space *)
  let ( ^^^ ) left right = left ^^ space ^^ right

  let cross = !^"×"

  let plus = !^"+"

  let minus = !^"-"

  let star = !^"*"

  let slash = !^"/"

  let unit_type = !^"()"

  let unit_term = !^"()"

  let arrow = !^"→"

  let box_type = !^"□"

  let nat_type = !^"Nat"

  (* keywords *)
  let fst_kwd = !^"π₁"

  let snd_kwd = !^"π₂"

  let box_kwd = !^"box"

  let fun_kwd = !^"λ"

  let let_kwd = !^"let"

  let letbox_kwd = !^"letbox"

  let in_kwd = !^"in"

  let parens_if b = if b then parens else fun x -> x

  let of_type =
    let open Type in
    let rec walk p = function
      | Unit -> unit_type
      | Nat -> nat_type
      | Base idT -> !^idT
      | Prod (t1, t2) -> parens_if (p > 1) (walk 1 t1 ^^ cross ^^ walk 2 t2)
      | Arr (dom, cod) -> parens_if (p > 0) (walk 1 dom ^^^ arrow ^^^ walk 0 cod)
      | Box t -> box_type ^^ walk 2 t
    in
    walk 0

  (** Pretty-print expressions with free vars substituited with
    their corresponding values from a regular environment *)
  let rec of_expr_with_free_vars renv expr =
    let open Expr in
    let rec walk p Location.{ data = e; _ } =
      match e with
      | Unit -> unit_term
      | Pair (e1, e2) -> angles (walk bvs 1 e1 ^^ comma ^/^ walk bvs 1 e2)
      | Fst pe -> group (parens (fst_kwd ^^ walk bvs 2 pe))
      | Snd pe -> group (parens (snd_kwd ^^ walk bvs 2 pe))
      | IntZ i -> !^(Nat.to_string i)
      | BinOp (op, e1, e2) ->
          let symb_op =
            match op with
            | Add -> plus
            | Sub -> minus
            | Mul -> star
            | Div -> slash
          in
          (parens_if (p > 1))
            (group (walk bvs 2 e1) ^^^ symb_op ^^^ walk bvs 1 e2)
      | VarL idl -> (
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
      | VarG idg -> !^(Id.M.to_string idg)
      | Fun (idl, t_of_id, body) ->
          (parens_if (p > 1))
            ( fun_kwd
            ^^ !^(Id.R.to_string idr)
            ^^^ colon ^^^ of_type ty_id ^^ dot ^^ space ^^ walk 1 body )
      | App { fe; arge } ->
          group ((parens_if (p >= 2)) (walk 2 fe ^/^ walk 2 arge))
      | Box { e } -> group ((parens_if (p >= 2)) (box_kwd ^^ space ^^ walk 2 e))
      | Let { idr; bound; body } ->
          (parens_if (p > 1))
            (group
               ( let_kwd
               ^^^ !^(Id.R.to_string idr)
               ^^^ equals ^^^ walk 2 bound ^^^ in_kwd ^/^ walk 1 body ))
      | Letbox { idm; boxed; body } ->
          (parens_if (p > 1))
            (group
               ( letbox_kwd
               ^^^ !^(Id.M.to_string idm)
               ^^^ equals ^^^ walk 2 boxed ^^^ in_kwd ^/^ walk 1 body ))
    in
    walk 0 expr

  (* This prints an expression as-is, i.e. no substitutions for free vars *)
  and of_expr e = of_expr_with_free_vars Env.R.emp e

  and of_val = function
    | Val.Unit -> unit_term
    | Val.IntZ i -> !^(Nat.to_string i)
    | Val.Pair (l1, l2) -> group (angles (of_lit l1 ^^ comma ^/^ of_lit l2))
    | Val.Clos (idl, body, lenv) ->
        fun_kwd
        ^^ !^(Id.R.to_string idr)
        ^^ dot
        ^^^ (* when print out closures, substitute the free vars in its body with
               the corresponding values from the closures' regular environment *)
        of_expr_with_free_vars env body
    | Val.Box { e } -> box_kwd ^^^ of_expr e
end
