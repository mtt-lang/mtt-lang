open Base
open Result.Let_syntax
open Ast

type error = string

let rec free_vars_m term =
  let open Expr in
  match term.Location.data with
  | Unit -> Set.empty (module Id.M)
  | Pair (e1, e2) -> Set.union (free_vars_m e1) (free_vars_m e2)
  | Fst pe | Snd pe -> free_vars_m pe
  | VarL _i -> Set.empty (module Id.M)
  | VarG i -> Set.singleton (module Id.M) i
  | Fun (_i, _t_of_id, body) -> free_vars_m body
  | App (fe, arge) -> Set.union (free_vars_m fe) (free_vars_m arge)
  | Box e -> free_vars_m e
  | Let (_i, bound_e, body) ->
      Set.union (free_vars_m bound_e) (free_vars_m body)
  | Letbox (i, boxed_e, body) ->
      Set.union (free_vars_m boxed_e)
        (Set.diff (free_vars_m body) (Set.singleton (module Id.M) i))

let refresh_m idg fvs =
  let rec loop (idg : Id.M.t) =
    if Set.mem fvs idg then loop (Id.M.mk (Id.M.to_string idg ^ "'")) else idg
    (* it's fresh enough already :) *)
  in
  if Set.mem fvs idg then Some (loop idg) else None

(* modal (modal) substitution *)
let rec subst_m term idg body =
  let data = body.Location.data in
  let loc = body.Location.loc in
  let open Expr in
  match data with
  | Unit -> body
  | Pair (e1, e2) ->
      Location.locate ~loc (Pair (subst_m term idg e1, subst_m term idg e2))
  | Fst pe -> Location.locate ~loc (Fst (subst_m term idg pe))
  | Snd pe -> Location.locate ~loc (Snd (subst_m term idg pe))
  | VarL _i -> body
  | VarG i -> if [%equal: Id.M.t] idg i then term else body
  | Fun (idl, t_of_id, body) ->
      Location.locate ~loc (Fun (idl, t_of_id, subst_m term idg body))
  | App (fe, arge) ->
      Location.locate ~loc (App (subst_m term idg fe, subst_m term idg arge))
  | Box e -> Location.locate ~loc (Box (subst_m term idg e))
  | Let (i, bound_e, body) ->
      Location.locate ~loc
        (Let (i, subst_m term idg bound_e, subst_m term idg body))
  | Letbox (i, boxed_e, body) ->
      Location.locate ~loc
        ( if [%equal: Id.M.t] idg i then
          Letbox (i, subst_m term idg boxed_e, body)
        else
          match refresh_m i (free_vars_m term) with
          | Some new_i ->
              let body_with_renamed_bound_var =
                subst_m (Location.locate ~loc (VarG new_i)) i body
              in
              Letbox
                ( new_i,
                  subst_m term idg boxed_e,
                  subst_m term idg body_with_renamed_bound_var )
          | None ->
              (* no need to rename the bound var *)
              Letbox (i, subst_m term idg boxed_e, subst_m term idg body) )

let rec eval_open gamma expr =
  let data = expr.Location.data in
  let loc = expr.Location.loc in
  let open Expr in
  match data with
  | Unit -> return (Location.locate ~loc Val.Unit)
  | Pair (e1, e2) ->
      let%map v1 = eval_open gamma e1 and v2 = eval_open gamma e2 in
      Location.locate ~loc (Val.Pair (v1, v2))
  | Fst pe -> (
      let%bind pv = eval_open gamma pe in
      match pv.data with
      | Val.Pair (v1, _v2) -> return v1
      | _ -> Result.fail @@ Location.pp ~msg:"fst is stuck;" pv.loc )
  | Snd pe -> (
      let%bind pv = eval_open gamma pe in
      match pv.data with
      | Val.Pair (_v1, v2) -> return v2
      | _ -> Result.fail @@ Location.pp ~msg:"snd is stuck" pv.loc )
  | VarL idl -> Env.lookup_r gamma idl
  | VarG _idg ->
      Result.fail
      @@ Location.pp
           ~msg:"Modal variable access is not possible in a well-typed term" loc
  | Fun (idl, _t_of_id, body) ->
      return @@ Location.locate ~loc (Val.Clos (idl, body, gamma))
  | App (fe, arge) -> (
      let%bind fv = eval_open gamma fe in
      let%bind argv = eval_open gamma arge in
      match fv.data with
      | Val.Clos (idl, body, c_gamma) ->
          eval_open (Env.extend_r c_gamma idl argv) body
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"Trying to apply an argument to a non-function"
               fv.loc )
  | Box e -> return @@ Location.locate ~loc (Val.Box e)
  | Let (idr, bound_e, body) ->
      let%bind bound_v = eval_open gamma bound_e in
      eval_open (Env.extend_r gamma idr bound_v) body
  | Letbox (idg, boxed_e, body) -> (
      let%bind boxed_v = eval_open gamma boxed_e in
      match boxed_v.data with
      | Val.Box e -> eval_open gamma (subst_m e idg body)
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"Trying to unbox a non-box expression" boxed_v.loc
      )

let eval expr = eval_open Env.emp_r expr
