open Base
open Result.Let_syntax
open Ast

type error = string

let rec free_vars_g term =
  let open Expr in
  match term with
  | Unit -> Set.empty (module Id.G)
  | Pair (e1, e2) -> Set.union (free_vars_g e1) (free_vars_g e2)
  | Fst pe | Snd pe -> free_vars_g pe
  | VarL _i -> Set.empty (module Id.G)
  | VarG i -> Set.singleton (module Id.G) i
  | Fun (_i, _t_of_id, body) -> free_vars_g body
  | App (fe, arge) -> Set.union (free_vars_g fe) (free_vars_g arge)
  | Box e -> free_vars_g e
  | Letbox (i, boxed_e, body) ->
      Set.union (free_vars_g boxed_e)
                (Set.diff (free_vars_g body) (Set.singleton (module Id.G) i))

let refresh_g idg fvs =
  let rec loop (idg : Id.G.t) =
    if Set.mem fvs idg then loop (Id.G.mk (Id.G.to_string idg ^ "'"))
    else idg (* it's fresh enough already :) *)
  in
  if Set.mem fvs idg then Some (loop idg) else None

(* modal (global) substitution *)
let rec subst_m term idg body =
  let open Expr in
  match body with
  | Unit -> body
  | Pair (e1, e2) -> Pair (subst_m term idg e1, subst_m term idg e2)
  | Fst pe -> Fst (subst_m term idg pe)
  | Snd pe -> Snd (subst_m term idg pe)
  | VarL _i -> body
  | VarG i -> if [%equal: Id.G.t] idg i then term else body
  | Fun (idl, t_of_id, body) -> Fun (idl, t_of_id, subst_m term idg body)
  | App (fe, arge) -> App (subst_m term idg fe, subst_m term idg arge)
  | Box e -> Box (subst_m term idg e)
  | Letbox (i, boxed_e, body) ->
    if [%equal: Id.G.t] idg i then
        Letbox (i, subst_m term idg boxed_e, body)
    else
      begin match refresh_g i (free_vars_g term) with
      | Some new_i ->
          let body_with_renamed_bound_var = subst_m (VarG new_i) i body in
          Letbox (new_i, subst_m term idg boxed_e, subst_m term idg body_with_renamed_bound_var)
      | None ->
          (* no need to rename the bound var *)
          Letbox (i, subst_m term idg boxed_e, subst_m term idg body)
      end

let rec eval_open gamma expr =
  let open Expr in
  match expr with
  | Unit ->
      return Lit.Unit
  | Pair (e1, e2) ->
      let%map v1 = eval_open gamma e1
      and     v2 = eval_open gamma e2 in
      Lit.Pair (v1, v2)
  | Fst pe ->
      let%bind pv = eval_open gamma pe in
      begin match pv with
      | Lit.Pair (v1, _v2) -> return v1
      | _ -> Result.fail "fst is stuck"
      end
  | Snd pe ->
      let%bind pv = eval_open gamma pe in
      begin match pv with
      | Lit.Pair (_v1, v2) -> return v2
      | _ -> Result.fail "snd is stuck"
      end
  | VarL idl ->
      Env.lookup_l gamma idl
  | VarG _idg ->
      Result.fail "Global variable access is not possible in a well-typed term"
  | Fun (idl, _t_of_id, body) ->
      return @@ Lit.Clos (idl, body, gamma)
  | App (fe, arge) ->
      let%bind fv = eval_open gamma fe in
      let%bind argv = eval_open gamma arge in
      begin match fv with
      | Lit.Clos (idl, body, c_gamma) -> eval_open (Env.extend_l c_gamma idl argv) body
      | _ -> Result.fail "Trying to apply an argument to a non-function"
      end
  | Box e ->
      return @@ Lit.Box e
  | Letbox (idg, boxed_e, body) ->
      let%bind boxed_v = eval_open gamma boxed_e in
      begin match boxed_v with
        | Lit.Box e -> eval_open gamma (subst_m e idg body)
      | _ -> Result.fail "Trying to unbox a non-box expression"
      end

let eval expr =
  eval_open Env.emp_l expr

