open Base
open Result.Let_syntax
open Ast
open Ast.Expr

type error =
  [ `TypeMismatchError of string
  | `UnboundRegularVarInsideBoxError of Location.t * string
  | Env.error ]

type 'e lerror = ([> error ] as 'e) Location.located

let check_equal ty1 ty2 msg =
  Result.ok_if_true ([%equal: Type.t] ty1 ty2) ~error:(`TypeMismatchError msg)

let with_error_location loc r =
  Result.map_error r ~f:(fun e -> Location.locate ~loc e)

let fail_in loc err = Result.fail @@ Location.locate ~loc @@ err

let rec check_open delta gamma Location.{ data = expr; loc } typ =
  match expr with
  | Unit ->
      with_error_location loc @@ check_equal typ Type.Unit "Expected unit type"
  | Pair { e1; e2 } -> (
      match typ with
      | Type.Prod { ty1; ty2 } ->
          let%map () = check_open delta gamma e1 ty1
          and () = check_open delta gamma e2 ty2 in
          ()
      | _ -> fail_in loc @@ `TypeMismatchError "Expected product type" )
  | Fst { e } -> (
      let%bind ty = infer_open delta gamma e in
      match ty with
      | Type.Prod { ty1; ty2 = _ } ->
          with_error_location loc
          @@ check_equal typ ty1
               "fst error: inferred type is different from the input one"
      | _ ->
          fail_in loc
          @@ `TypeMismatchError "fst is applied to a non-product type" )
  | Snd { e } -> (
      let%bind ty = infer_open delta gamma e in
      match ty with
      | Type.Prod { ty1 = _; ty2 } ->
          with_error_location loc
          @@ check_equal typ ty2
               "snd error: inferred type is different from the input one"
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"snd is applied to a non-product type" loc )
  | Nat _ ->
      Result.ok_if_true
        ([%equal: Type.t] typ Type.Nat)
        ~error:(Location.pp ~msg:"expected nat type" loc)
  | BinOp { op = _; e1; e2 } ->
      let%bind ty1 = infer_open delta gamma e1 in
      let%bind ty2 = infer_open delta gamma e2 in
      Result.ok_if_true
        ([%equal: Type.t] ty1 Type.Nat && [%equal: Type.t] ty2 Type.Nat)
        ~error:
          (Location.pp ~msg:"binary operator's operands must be a numbers" loc)
  | VarR { idr } ->
      let%bind ty = Env.R.lookup gamma idr in
      Result.ok_if_true
        ([%equal: Type.t] typ ty)
        ~error:(Location.pp ~msg:"Unexpected regular variable type" loc)
  | VarG idg ->
      let%bind ty = Env.lookup_m delta idg in
      Result.ok_if_true
        ([%equal: Type.t] typ ty)
        ~error:(Location.pp ~msg:"Unexpected modal variable type" loc)
  | Fun (idl, t_of_id, body) -> (
      match typ with
      | Type.Arr (dom, cod) ->
          if [%equal: Type.t] dom t_of_id then
            check_open delta (Env.extend_r gamma idl dom) body cod
          else
            Result.fail
            @@ Location.pp
                 ~msg:
                   "Domain of arrow type is not the same as type of function \
                    parameter"
                 loc
      | _ -> Result.fail @@ Location.pp ~msg:"Arror type expected" loc )
  | Fix _ -> Result.fail @@ Location.pp ~msg:"Fix hasn't been implemented yet" loc
  | App (fe, arge) -> (
      let%bind ty = infer_open delta gamma fe in
      match ty with
      | Type.Arr { dom; cod } ->
          let%bind () = check_open delta gamma arge dom in
          with_error_location loc
          @@ check_equal typ cod "Unexpected function codomain"
      | _ ->
          fail_in loc @@ `TypeMismatchError "Inferred type is not an arrow type"
      )
  | Box { e } -> (
      match typ with
      | Type.Box { ty } -> (
          match check_open delta Env.R.emp e ty with
          | Error
              { data = `EnvUnboundVariableError (var_name, _); loc = var_loc }
            ->
              fail_in loc
              @@ `UnboundRegularVarInsideBoxError
                   ( var_loc,
                     [%string
                       "regular variable $(var_name) (bound at \
                        $(Location.pp_column_range var_loc)) cannot accessed \
                        from boxed expression"] )
          | x -> x )
      | _ -> fail_in loc @@ `TypeMismatchError "Error: unboxed type" )
  | Let { idr; bound; body } ->
      let%bind ty = infer_open delta gamma bound in
      check_open delta (Env.R.extend gamma idr ty) body typ
  | Letbox { idm; boxed; body } -> (
      let%bind ty = infer_open delta gamma boxed in
      match ty with
      | Type.Box { ty } -> check_open (Env.M.extend delta idm ty) gamma body typ
      | _ -> fail_in loc @@ `TypeMismatchError "Inferred type is not a box" )

and infer_open delta gamma Location.{ data = expr; loc } =
  match expr with
  | Unit -> return Type.Unit
  | Pair { e1; e2 } ->
      let%map ty1 = infer_open delta gamma e1
      and ty2 = infer_open delta gamma e2 in
      Type.Prod { ty1; ty2 }
  | Fst { e } -> (
      let%bind ty = infer_open delta gamma e in
      match ty with
      | Type.Prod { ty1; ty2 = _ } -> return ty1
      | _ ->
          fail_in loc
          @@ `TypeMismatchError "fst is applied to a non-product type" )
  | Snd { e } -> (
      let%bind ty = infer_open delta gamma e in
      match ty with
      | Type.Prod { ty1 = _; ty2 } -> return ty2
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"snd is applied to a non-product type" loc )
  | Nat _ -> return Type.Nat
  | BinOp { op = _; e1; e2 } -> (
      let%bind ty1 = infer_open delta gamma e1 in
      let%bind ty2 = infer_open delta gamma e2 in
      match (ty1, ty2) with
      | Type.Nat, Type.Nat -> return Type.Nat
      | _, _ ->
          Result.fail
          @@ Location.pp ~msg:"binary operator's operands must be numbers" loc )
  | VarR { idr } ->
      Env.R.lookup gamma idr
      |> Result.map_error ~f:(fun msg -> Location.pp ~msg loc)
  | VarM { idm } ->
      Env.M.lookup delta idm
      |> Result.map_error ~f:(fun msg -> Location.pp ~msg loc)
  | Fun { idr; ty_id; body } ->
      let%map ty_body = infer_open delta (Env.R.extend gamma idr ty_id) body in
      Type.Arr { dom = ty_id; cod = ty_body }
  | App { fe; arge } -> (
      let%bind ty = infer_open delta gamma fe in
      match ty with
      | Type.Arr { dom; cod } ->
          let%bind () = check_open delta gamma arge dom in
          return cod
      | _ ->
          fail_in loc @@ `TypeMismatchError "Inferred type is not an arrow type"
      )
  | Box { e } ->
      let%map ty =
        match infer_open delta Env.R.emp e with
        | Error { data = `EnvUnboundVariableError (var_name, _); loc = var_loc }
          ->
            fail_in loc
            @@ `UnboundRegularVarInsideBoxError
                 ( var_loc,
                   [%string
                     "regular variable $(var_name) (bound at \
                      $(Location.pp_column_range var_loc)) cannot accessed \
                      from boxed expression"] )
        | x -> x
      in
      Type.Box { ty }
  | Let { idr; bound; body } ->
      let%bind ty = infer_open delta gamma bound in
      infer_open delta (Env.R.extend gamma idr ty) body
  | Letbox { idm; boxed; body } -> (
      let%bind tyb = infer_open delta gamma boxed in
      match tyb with
      | Type.Box { ty } -> infer_open (Env.M.extend delta idm ty) gamma body
      | _ -> fail_in loc @@ `TypeMismatchError "Inferred type is not a box" )

let check expr typ = check_open Env.M.emp Env.R.emp expr typ

let infer expr = infer_open Env.M.emp Env.R.emp expr
