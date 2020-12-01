open Base
open Result.Let_syntax
open Ast
open Ast.Expr

type error = string

let rec check_open delta gamma Location.{ data = expr; loc } typ =
  match expr with
  | Unit ->
      Result.ok_if_true
        ([%equal: Type.t] typ Type.Unit)
        ~error:(Location.pp ~msg:"Expected unit type" loc)
  | Pair (e1, e2) -> (
      match typ with
      | Type.Prod (ty1, ty2) ->
          let%map () = check_open delta gamma e1 ty1
          and () = check_open delta gamma e2 ty2 in
          ()
      | _ -> Result.fail @@ Location.pp ~msg:"Expected product type" loc )
  | Fst pe -> (
      let%bind ty = infer_open delta gamma pe in
      match ty with
      | Type.Prod (ty1, _ty2) ->
          Result.ok_if_true
            ([%equal: Type.t] typ ty1)
            ~error:
              (Location.pp
                 ~msg:"fst error: inferred type is different from the input one"
                 loc)
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"fst is applied to a non-product type" loc )
  | Snd pe -> (
      let%bind ty = infer_open delta gamma pe in
      match ty with
      | Type.Prod (_ty1, ty2) ->
          Result.ok_if_true
            ([%equal: Type.t] typ ty2)
            ~error:
              (Location.pp
                 ~msg:"snd error: inferred type is different from the input one"
                 loc)
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"snd is applied to a non-product type" loc )
  | VarR idr ->
      let%bind ty = Env.R.lookup gamma idr in
      Result.ok_if_true
        ([%equal: Type.t] typ ty)
        ~error:(Location.pp ~msg:"Unexpected regular variable type" loc)
  | VarM idm ->
      let%bind ty = Env.M.lookup delta idm in
      Result.ok_if_true
        ([%equal: Type.t] typ ty)
        ~error:(Location.pp ~msg:"Unexpected modal variable type" loc)
  | Fun (idr, t_of_id, body) -> (
      match typ with
      | Type.Arr (dom, cod) ->
          if [%equal: Type.t] dom t_of_id then
            check_open delta (Env.R.extend gamma idr dom) body cod
          else
            Result.fail
            @@ Location.pp
                 ~msg:
                   "Domain of arrow type is not the same as type of function \
                    parameter"
                 loc
      | _ -> Result.fail @@ Location.pp ~msg:"Arror type expected" loc )
  | App (fe, arge) -> (
      let%bind ty = infer_open delta gamma fe in
      match ty with
      | Type.Arr (dom, cod) ->
          let%bind () = check_open delta gamma arge dom in
          Result.ok_if_true
            ([%equal: Type.t] typ cod)
            ~error:(Location.pp ~msg:"Unexpected function codomain" loc)
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"Inferred type is not an arrow type" loc )
  | Box e -> (
      match typ with
      | Type.Box t -> check_open delta Env.R.emp e t
      | _ -> Result.fail @@ Location.pp ~msg:"Error: unboxed type" loc )
  | Let (idr, bound_e, body) ->
      let%bind ty = infer_open delta gamma bound_e in
      check_open delta (Env.R.extend gamma idr ty) body typ
  | Letbox (idm, boxed_e, body) -> (
      let%bind ty = infer_open delta gamma boxed_e in
      match ty with
      | Type.Box t -> check_open (Env.M.extend delta idm t) gamma body typ
      | _ -> Result.fail @@ Location.pp ~msg:"Inferred type is not a box" loc )

and infer_open delta gamma Location.{ data = expr; loc } =
  match expr with
  | Unit -> return Type.Unit
  | Pair (e1, e2) ->
      let%map ty1 = infer_open delta gamma e1
      and ty2 = infer_open delta gamma e2 in
      Type.Prod (ty1, ty2)
  | Fst pe -> (
      let%bind ty = infer_open delta gamma pe in
      match ty with
      | Type.Prod (ty1, _ty2) -> return ty1
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"fst is applied to a non-product type" loc )
  | Snd pe -> (
      let%bind ty = infer_open delta gamma pe in
      match ty with
      | Type.Prod (_ty1, ty2) -> return ty2
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"snd is applied to a non-product type" loc )
  | VarR idr ->
      Env.R.lookup gamma idr
      |> Result.map_error ~f:(fun msg -> Location.pp ~msg loc)
  | VarM idm ->
      Env.M.lookup delta idm
      |> Result.map_error ~f:(fun msg -> Location.pp ~msg loc)
  | Fun (idr, dom, body) ->
      let%map cod = infer_open delta (Env.R.extend gamma idr dom) body in
      Type.Arr (dom, cod)
  | App (fe, arge) -> (
      let%bind ty = infer_open delta gamma fe in
      match ty with
      | Type.Arr (dom, cod) ->
          let%bind () = check_open delta gamma arge dom in
          return cod
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"Inferred type is not an arrow type" loc )
  | Box e ->
      let%map ty = infer_open delta Env.R.emp e in
      Type.Box ty
  | Let (idr, bound_e, body) ->
      let%bind ty = infer_open delta gamma bound_e in
      infer_open delta (Env.R.extend gamma idr ty) body
  | Letbox (idm, boxed_e, body) -> (
      let%bind ty = infer_open delta gamma boxed_e in
      match ty with
      | Type.Box t -> infer_open (Env.M.extend delta idm t) gamma body
      | _ -> Result.fail @@ Location.pp ~msg:"Inferred type is not a box" loc )

let check expr typ = check_open Env.M.emp Env.R.emp expr typ

let infer expr = infer_open Env.M.emp Env.R.emp expr
