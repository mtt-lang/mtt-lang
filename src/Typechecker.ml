open Base
open Result.Let_syntax
open Ast
open Ast.Expr

type error = [
  | `TypeMismatchError of string
  | `UnboundRegularVarInsideBoxError of Location.t (* var loc *)
                                        * string (* message *)
  | Env.error
]

type 'e lerror = ([> error ] as 'e) Location.located

let check_equal ty1 ty2 msg =
  Result.ok_if_true ([%equal: Type.t] ty1 ty2) ~error:(`TypeMismatchError msg)

let with_error_location loc r =
  Result.map_error r ~f:(fun e -> Location.locate ~loc:loc e)

let fail_in loc err = Result.fail @@ Location.locate ~loc:loc @@ err

let rec check_open delta gamma Location.{ data = expr; loc } typ =
  match expr with
  | Unit -> with_error_location loc
      @@ check_equal typ Type.Unit "Expected unit type"
  | Pair { e1; e2 } -> (
      match typ with
      | Type.Prod { ty1; ty2 } ->
          let%map () = check_open delta gamma e1 ty1
          and () = check_open delta gamma e2 ty2 in
          ()
      | _ -> fail_in loc
          @@ `TypeMismatchError "Expected product type")
  | Fst { e } -> (
      let%bind ty = infer_open delta gamma e in
      match ty with
      | Type.Prod { ty1; ty2 = _ } ->
          with_error_location loc
          @@ check_equal typ ty1 "fst error: inferred type is different from the input one"
      | _ -> fail_in loc
          @@ `TypeMismatchError "fst is applied to a non-product type")
  | Snd { e } -> (
      let%bind ty = infer_open delta gamma e in
      match ty with
      | Type.Prod { ty1 = _; ty2 } ->
          with_error_location loc
          @@ check_equal typ ty2  "snd error: inferred type is different from the input one"
      | _ -> fail_in loc
          @@ `TypeMismatchError "snd is applied to a non-product type")
  | VarR { idr } ->
      let%bind ty = with_error_location loc @@ Env.R.lookup gamma idr in
      with_error_location loc
      @@ check_equal typ ty "Unexpected regular variable type"
  | VarM { idm } ->
      let%bind ty = with_error_location loc @@ Env.M.lookup delta idm in
      with_error_location loc 
      @@ check_equal typ ty "Unexpected modal variable type"
  | Fun { idr; ty_id; body } -> (
      match typ with
      | Type.Arr { dom; cod } ->
          let%bind () = with_error_location loc @@ check_equal dom ty_id
              "Domain of arrow type is not the same as type of function parameter" in
          check_open delta (Env.R.extend gamma idr dom) body cod
      | _ -> fail_in loc
          @@ `TypeMismatchError "Arrow type expected")
  | App { fe; arge } -> (
      let%bind ty = infer_open delta gamma fe in
      match ty with
      | Type.Arr { dom; cod } ->
          let%bind () = check_open delta gamma arge dom in
          with_error_location loc
          @@ check_equal typ cod "Unexpected function codomain"
      | _ -> fail_in loc
          @@ `TypeMismatchError "Inferred type is not an arrow type")
  | Box { e } -> (
      match typ with
      | Type.Box { ty } -> (
          match check_open delta Env.R.emp e ty with
          | Error ({data = (`EnvUnboundVariableError (var_name, _)); loc = var_loc}) -> fail_in loc
              @@ `UnboundRegularVarInsideBoxError
                (var_loc, [%string "regular variable $(var_name) \
                                    (bound at $(Location.pp_column_range var_loc)) \
                                    cannot accessed from boxed expression"])
          | x -> x)
      | _ -> fail_in loc
          @@ `TypeMismatchError "Error: unboxed type")
  | Let { idr; bound; body } ->
      let%bind ty = infer_open delta gamma bound in
      check_open delta (Env.R.extend gamma idr ty) body typ
  | Letbox { idm; boxed; body } -> (
      let%bind ty = infer_open delta gamma boxed in
      match ty with
      | Type.Box { ty } -> check_open (Env.M.extend delta idm ty) gamma body typ
      | _ -> fail_in loc
          @@ `TypeMismatchError "Inferred type is not a box")

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
      | _ -> fail_in loc
          @@ `TypeMismatchError "fst is applied to a non-product type")
  | Snd { e } -> (
      let%bind ty = infer_open delta gamma e in
      match ty with
      | Type.Prod { ty1 = _; ty2 } -> return ty2
      | _ -> fail_in loc
          @@ `TypeMismatchError "snd is applied to a non-product type" )
  | VarR { idr } -> with_error_location loc @@ Env.R.lookup gamma idr
  | VarM { idm } -> with_error_location loc @@ Env.M.lookup delta idm
  | Fun { idr; ty_id; body } ->
      let%map ty_body = infer_open delta (Env.R.extend gamma idr ty_id) body in
      Type.Arr { dom = ty_id; cod = ty_body }
  | App { fe; arge } -> (
      let%bind ty = infer_open delta gamma fe in
      match ty with
      | Type.Arr { dom; cod } ->
          let%bind () = check_open delta gamma arge dom in
          return cod
      | _ -> fail_in loc
          @@ `TypeMismatchError "Inferred type is not an arrow type" )
  | Box { e } ->
      let%map ty = (match infer_open delta Env.R.emp e with
        | Error ({data = (`EnvUnboundVariableError (var_name, _)); loc = var_loc}) -> fail_in loc
            @@ `UnboundRegularVarInsideBoxError
              (var_loc, [%string "regular variable $(var_name) \
                              (bound at $(Location.pp_column_range var_loc)) \
                              cannot accessed from boxed expression"])
        | x -> x)
      in Type.Box { ty }
  | Let { idr; bound; body } ->
      let%bind ty = infer_open delta gamma bound in
      infer_open delta (Env.R.extend gamma idr ty) body
  | Letbox { idm; boxed; body } -> (
      let%bind tyb = infer_open delta gamma boxed in
      match tyb with
      | Type.Box { ty } -> infer_open (Env.M.extend delta idm ty) gamma body
      | _ -> fail_in loc
          @@ `TypeMismatchError "Inferred type is not a box" )

let check expr typ = check_open Env.M.emp Env.R.emp expr typ

let infer expr = infer_open Env.M.emp Env.R.emp expr
