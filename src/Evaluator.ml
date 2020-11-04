open Base
open Result.Let_syntax
open Ast

type error = string

let rec free_vars_m Location.{ data = term; _ } =
  let open Expr in
  match term with
  | Unit -> Set.empty (module Id.M)
  | Pair (e1, e2) -> Set.union (free_vars_m e1) (free_vars_m e2)
  | Fst pe | Snd pe -> free_vars_m pe
  | VarR _i -> Set.empty (module Id.M)
  | VarM i -> Set.singleton (module Id.M) i
  | Fun (_i, _t_of_id, body) -> free_vars_m body
  | App (fe, arge) -> Set.union (free_vars_m fe) (free_vars_m arge)
  | Box e -> free_vars_m e
  | Let (_i, bound_e, body) ->
      Set.union (free_vars_m bound_e) (free_vars_m body)
  | Letbox (i, boxed_e, body) ->
      Set.union (free_vars_m boxed_e)
        (Set.diff (free_vars_m body) (Set.singleton (module Id.M) i))

let refresh_m idm fvs =
  let rec loop (idm : Id.M.t) =
    if Set.mem fvs idm then loop (Id.M.mk (Id.M.to_string idm ^ "'")) else idm
    (* it's fresh enough already :) *)
  in
  if Set.mem fvs idm then Some (loop idm) else None

(* modal (modal) substitution *)
let rec subst_m term idm Location.{ data = body; _ } =
  let open Expr in
  match body with
  | Unit -> Location.locate body
  | Pair (e1, e2) ->
      Location.locate (Pair (subst_m term idm e1, subst_m term idm e2))
  | Fst pe -> Location.locate (Fst (subst_m term idm pe))
  | Snd pe -> Location.locate (Snd (subst_m term idm pe))
  | VarR _i -> Location.locate body
  | VarM i -> if [%equal: Id.M.t] idm i then term else Location.locate body
  | Fun (idr, t_of_id, body) ->
      Location.locate (Fun (idr, t_of_id, subst_m term idm body))
  | App (fe, arge) ->
      Location.locate (App (subst_m term idm fe, subst_m term idm arge))
  | Box e -> Location.locate (Box (subst_m term idm e))
  | Let (idr, bound_e, body) ->
      Location.locate
        (Let (idr, subst_m term idm bound_e, subst_m term idm body))
  | Letbox (i, boxed_e, body) ->
      Location.locate
        ( if [%equal: Id.M.t] idm i then
          Letbox (i, subst_m term idm boxed_e, body)
        else
          match refresh_m i (free_vars_m term) with
          | Some new_i ->
              let body_with_renamed_bound_var =
                subst_m (Location.locate (VarM new_i)) i body
              in
              Letbox
                ( new_i,
                  subst_m term idm boxed_e,
                  subst_m term idm body_with_renamed_bound_var )
          | None ->
              (* no need to rename the bound var *)
              Letbox (i, subst_m term idm boxed_e, subst_m term idm body) )

let rec eval_open gamma Location.{ data = expr; _ } =
  let open Expr in
  match expr with
  | Unit -> return Val.Unit
  | Pair (e1, e2) ->
      let%map v1 = eval_open gamma e1 and v2 = eval_open gamma e2 in
      Val.Pair (v1, v2)
  | Fst pe -> (
      let%bind pv = eval_open gamma pe in
      match pv with
      | Val.Pair (v1, _v2) -> return v1
      | _ -> Result.fail "fst is stuck" )
  | Snd pe -> (
      let%bind pv = eval_open gamma pe in
      match pv with
      | Val.Pair (_v1, v2) -> return v2
      | _ -> Result.fail "snd is stuck" )
  | VarR idr -> Env.R.lookup gamma idr
  | VarM _idm ->
      Result.fail "Modal variable access is not possible in a well-typed term"
  | Fun (idr, _t_of_id, body) -> return @@ Val.Clos (idr, body, gamma)
  | App (fe, arge) -> (
      let%bind fv = eval_open gamma fe in
      let%bind argv = eval_open gamma arge in
      match fv with
      | Val.Clos (idr, body, c_gamma) ->
          eval_open (Env.R.extend c_gamma idr argv) body
      | _ -> Result.fail "Trying to apply an argument to a non-function" )
  | Box e -> return @@ Val.Box e
  | Let (idr, bound_e, body) ->
      let%bind bound_v = eval_open gamma bound_e in
      eval_open (Env.R.extend gamma idr bound_v) body
  | Letbox (idm, boxed_e, body) -> (
      let%bind boxed_v = eval_open gamma boxed_e in
      match boxed_v with
      | Val.Box e -> eval_open gamma (subst_m e idm body)
      | _ -> Result.fail "Trying to unbox a non-box expression" )

let eval expr = eval_open Env.R.emp expr
