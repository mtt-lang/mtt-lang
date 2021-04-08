open Base
open Result.Let_syntax
open Ast

type error = [ `EvaluationError of string | Env.error ]

let rec free_vars_m Location.{ data = term; _ } =
  let open Expr in
  match term with
  | Unit -> Set.empty (module Id.M)
  | Pair (e1, e2) -> Set.union (free_vars_m e1) (free_vars_m e2)
  | Fst pe | Snd pe -> free_vars_m pe
  | Nat _n -> Set.empty (module Id.M)
  | BinOp (_op, e1, e2) -> Set.union (free_vars_m e1) (free_vars_m e2)
  | VarL _i -> Set.empty (module Id.M)
  | VarG i -> Set.singleton (module Id.M) i
  | Fun (_i, _t_of_id, body) -> free_vars_m body
  | Fix (_i, _t_of_id, _f_i, body) -> free_vars_m body
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
let rec subst_m term identm Location.{ data = body; _ } =
  let open Expr in
  match body with
  | Unit -> Location.locate body
  | Pair (e1, e2) ->
      Location.locate (Pair (subst_m term idg e1, subst_m term idg e2))
  | Fst pe -> Location.locate (Fst (subst_m term idg pe))
  | Snd pe -> Location.locate (Snd (subst_m term idg pe))
  | Nat _n -> Location.locate body
  | BinOp (op, e1, e2) ->
      Location.locate (BinOp (op, subst_m term idg e1, subst_m term idg e2))
  | VarL _i -> Location.locate body
  | VarG i -> if [%equal: Id.M.t] idg i then term else Location.locate body
  | Fun (idl, t_of_id, body) ->
      Location.locate (Fun (idl, t_of_id, subst_m term idg body))
  | Fix (idl, t_of_id, f_idl, body) ->
      Location.locate (Fix ((idl, t_of_id, f_idl, subst_m term idg body)))
  | App (fe, arge) ->
      Location.locate (App (subst_m term idg fe, subst_m term idg arge))
  | Box e -> Location.locate (Box (subst_m term idg e))
  | Let (i, bound_e, body) ->
      Location.locate (Let (i, subst_m term idg bound_e, subst_m term idg body))
  | Letbox (i, boxed_e, body) ->
      Location.locate
        ( if [%equal: Id.M.t] identm idm then
          Letbox { idm; boxed = subst_m term identm boxed; body }
        else
          match refresh_m idm (free_vars_m term) with
          | Some new_i ->
              let body_with_renamed_bound_var =
                subst_m (var_m new_i) idm body
              in
              Letbox
                {
                  idm = new_i;
                  boxed = subst_m term identm boxed;
                  body = subst_m term identm body_with_renamed_bound_var;
                }
          | None ->
              (* no need to rename the bound var *)
              Letbox
                {
                  idm;
                  boxed = subst_m term identm boxed;
                  body = subst_m term identm body;
                } )

let rec eval_open gamma Location.{ data = expr; _ } =
  let open Expr in
  match expr with
  | Unit -> return Val.Unit
  | Pair { e1; e2 } ->
      let%map v1 = eval_open gamma e1 and v2 = eval_open gamma e2 in
      Val.Pair { v1; v2 }
  | Fst { e } -> (
      let%bind pv = eval_open gamma e in
      match pv with
      | Val.Pair { v1; v2 = _ } -> return v1
      | _ -> Result.fail @@ `EvaluationError "fst is stuck" )
  | Snd { e } -> (
      let%bind pv = eval_open gamma e in
      match pv with
      | Val.Pair (_v1, v2) -> return v2
      | _ -> Result.fail "snd is stuck" )
  | Nat n -> return @@ Val.Nat n
  | BinOp (op, e1, e2) -> (
      let%bind lhs = eval_open gamma e1 in
      let%bind rhs = eval_open gamma e2 in
      match (lhs, rhs) with
      | Val.Nat n1, Val.Nat n2 -> (
          match op with
          | Add -> return @@ Val.Nat (Nat.add n1 n2)
          | Sub -> return @@ Val.Nat (Nat.sub n1 n2)
          | Mul -> return @@ Val.Nat (Nat.mul n1 n2)
          | Div -> return @@ Val.Nat (Nat.div n1 n2) )
      | _, _ -> Result.fail "Only numbers can be multiplied" )
  | VarL idl -> Env.lookup_r gamma idl
  | VarG _idg ->
      Result.fail "Modal variable access is not possible in a well-typed term"
  | Fun (idl, _t_of_id, body) -> return @@ Val.Clos (idl, body, gamma)
  | Fix (idl, _t_of_id, f_idl, body) -> return @@ Val.ReClos (idl, f_idl, body, gamma)
  | App (fe, arge) -> (
      let%bind fv = eval_open gamma fe in
      let%bind argv = eval_open gamma arge in
      match fv with
      | Val.Clos (idl, body, c_gamma) ->
          eval_open (Env.extend_r c_gamma idl argv) body
      | Val.ReClos (idl, f_idl, body, c_gamma) ->
          let fix_gamma = (Env.extend_r c_gamma idl fv) in
          eval_open (Env.extend_r fix_gamma f_idl argv) body
      | _ -> Result.fail "Trying to apply an argument to a non-function" )
  | Box e -> return @@ Val.Box e
  | Let (idr, bound_e, body) ->
      let%bind bound_v = eval_open gamma bound_e in
      eval_open (Env.extend_r gamma idr bound_v) body
  | Letbox (idg, boxed_e, body) -> (
      let%bind boxed_v = eval_open gamma boxed_e in
      match boxed_v with
      | Val.Box { e } -> eval_open gamma (subst_m e idm body)
      | _ ->
          Result.fail @@ `EvaluationError "Trying to unbox a non-box expression"
      )

let eval expr = eval_open Env.R.emp expr
