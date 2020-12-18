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

  let fix_kwd = !^"let fix"

  let letbox_kwd = !^"letbox"

  let in_kwd = !^"in"

  let parens_if b = if b then parens else fun x -> x

  let of_type =
    let open Type in
    let rec walk p = function
      | Unit -> unit_type
      | Base { idt } -> !^idt
      | Prod { ty1; ty2 } ->
          parens_if (p > 1) (walk 1 ty1 ^^ cross ^^ walk 2 ty2)
      | Arr { dom; cod } ->
          parens_if (p > 0) (walk 1 dom ^^^ arrow ^^^ walk 0 cod)
      | Box { ty } -> box_type ^^ walk 2 ty
    in
    walk 0

  (** Pretty-print expressions with free vars substituited with
    their corresponding values from a regular environment *)
  let rec of_expr_with_free_vars renv expr =
    let open Expr in
    let rec walk p Location.{ data = e; _ } =
      match e with
      | Unit -> unit_term
      | Pair { e1; e2 } -> angles (walk 1 e1 ^^ comma ^/^ walk 1 e2)
      | Fst { e } -> group (parens (fst_kwd ^^ walk 2 e))
      | Snd { e } -> group (parens (snd_kwd ^^ walk 2 e))
      | VarR { idr } -> (
          match Env.R.lookup renv idr with
          | Ok v -> parens (of_val v)
          | Error _ -> !^(Id.R.to_string idr) )
      | VarM { idm } -> !^(Id.M.to_string idm)
      | Fun { idr; ty_id; body } ->
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
      | Fix { idr; bound; body } ->
               (parens_if (p > 1))
                 (group
                    ( fix_kwd
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
    | Val.Pair { v1; v2 } -> group (angles (of_val v1 ^^ comma ^/^ of_val v2))
    | Val.Clos { idr; body; env } ->
        fun_kwd
        ^^ !^(Id.R.to_string idr)
        ^^ dot
        ^^^ (* when print out closures, substitute the free vars in its body with
               the corresponding values from the closures' regular environment *)
        of_expr_with_free_vars env body
    | Val.Box { e } -> box_kwd ^^^ of_expr e
end
