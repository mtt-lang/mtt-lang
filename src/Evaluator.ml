open Base
open Result.Let_syntax
open Ast
open Location

type error = string

let rec free_vars_g term =
  let open Expr in
  match term.data with
  | Unit -> Set.empty (module Id.M)
  | Pair (e1, e2) -> Set.union (free_vars_g e1) (free_vars_g e2)
  | Fst pe | Snd pe -> free_vars_g pe
  | VarL _i -> Set.empty (module Id.M)
  | VarG i -> Set.singleton (module Id.M) i
  | Fun (_i, _t_of_id, body) -> free_vars_g body
  | App (fe, arge) -> Set.union (free_vars_g fe) (free_vars_g arge)
  | Box e -> free_vars_g e
  | Letbox (i, boxed_e, body) ->
      Set.union (free_vars_g boxed_e)
        (Set.diff (free_vars_g body) (Set.singleton (module Id.M) i))

let refresh_g idg fvs =
  let rec loop (idg : Id.M.t) =
    if Set.mem fvs idg then loop (Id.M.mk (Id.M.to_string idg ^ "'")) else idg
    (* it's fresh enough already :) *)
  in
  if Set.mem fvs idg then Some (loop idg) else None

(* modal (modal) substitution *)
let rec subst_m term idg body =
  let open Expr in
  match body.data with
  | Unit -> body
  | Pair (e1, e2) -> mkLocation (Pair (subst_m term idg e1, subst_m term idg e2)) body.loc
  | Fst pe -> mkLocation (Fst (subst_m term idg pe)) body.loc
  | Snd pe -> mkLocation (Snd (subst_m term idg pe)) body.loc
  | VarL _i -> body
  | VarG i -> if [%equal: Id.M.t] idg i then term else body
  | Fun (idl, t_of_id, body) -> mkLocation (Fun (idl, t_of_id, subst_m term idg body)) body.loc
  | App (fe, arge) -> mkLocation (App (subst_m term idg fe, subst_m term idg arge)) body.loc
  | Box e -> mkLocation (Box (subst_m term idg e)) body.loc
  | Letbox (i, boxed_e, body) -> mkLocation (
      if [%equal: Id.M.t] idg i then Letbox (i, subst_m term idg boxed_e, body)
      else
        match refresh_g i (free_vars_g term) with
        | Some new_i ->
            let body_with_renamed_bound_var = subst_m (mkLocation (VarG new_i) body.loc) i body in
            Letbox
              ( new_i,
                subst_m term idg boxed_e,
                subst_m term idg body_with_renamed_bound_var )
        | None ->
            (* no need to rename the bound var *)
            Letbox (i, subst_m term idg boxed_e, subst_m term idg body) ) body.loc

let rec eval_open gamma expr =
  let open Expr in
  match expr.data with
  | Unit -> return (mkLocation Val.Unit expr.loc)
  | Pair (e1, e2) ->
      let%map v1 = eval_open gamma e1 and v2 = eval_open gamma e2 in
      mkLocation (Val.Pair (v1, v2)) expr.loc
  | Fst pe -> (
      let%bind pv = eval_open gamma pe in
      match pv.data with
      | Val.Pair (v1, _v2) -> return v1
      | _ -> Result.fail @@ errorMsg "fst is stuck;" pv 
      )
  | Snd pe -> (
      let%bind pv = eval_open gamma pe in
      match pv.data with
      | Val.Pair (_v1, v2) -> return v2
      | _ -> Result.fail @@ errorMsg "snd is stuck" pv 
      )
  | VarL idl -> Env.lookup_l gamma idl
  | VarG _idg ->
      Result.fail 
        @@ errorMsg 
        "Modal variable access is not possible in a well-typed term"
        expr 
  | Fun (idl, _t_of_id, body) -> return @@ mkLocation (Val.Clos (idl, body, gamma)) expr.loc
  | App (fe, arge) -> (
      let%bind fv = eval_open gamma fe in
      let%bind argv = eval_open gamma arge in
      match fv.data with
      | Val.Clos (idl, body, c_gamma) ->
          eval_open (Env.extend_l c_gamma idl argv) body
      | _ -> 
      Result.fail 
        @@ errorMsg 
          "Trying to apply an argument to a non-function"
          fv  
      )
  | Box e -> return @@ mkLocation (Val.Box e) expr.loc
  | Letbox (idg, boxed_e, body) -> (
      let%bind boxed_v = eval_open gamma boxed_e in
      match boxed_v.data with
      | Val.Box e -> eval_open gamma (subst_m e idg body)
      | _ -> 
        Result.fail @@ errorMsg "Trying to unbox a non-box expression" boxed_v )

let eval expr = eval_open Env.emp_l expr
