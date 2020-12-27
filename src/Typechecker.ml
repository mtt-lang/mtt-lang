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

let with_error_location loc r =
  Result.map_error r ~f:(fun e -> Location.locate ~loc:loc e)

let fail_in loc err = Result.fail @@ Location.locate ~loc:loc @@ err

let rec check_open delta gamma Location.{ data = expr; loc } typ =
  match expr with
  | Unit ->
      Result.ok_if_true
        ([%equal: Type.t] typ Type.Unit)
        ~error:(Location.locate ~loc:loc @@ `TypeMismatchError "Expected unit type")
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
          Result.ok_if_true
            ([%equal: Type.t] typ ty1)
            ~error:(Location.locate ~loc:loc
                    @@ `TypeMismatchError "fst error: inferred type is different from the input one")
      | _ -> fail_in loc
          @@ `TypeMismatchError "fst is applied to a non-product type")
  | Snd { e } -> (
      let%bind ty = infer_open delta gamma e in
      match ty with
      | Type.Prod { ty1 = _; ty2 } ->
          Result.ok_if_true
            ([%equal: Type.t] typ ty2)
            ~error:(Location.locate ~loc:loc
                    @@ `TypeMismatchError "snd error: inferred type is different from the input one")
      | _ -> fail_in loc
          @@ `TypeMismatchError "snd is applied to a non-product type")
  | VarR { idr } ->
      let%bind ty = with_error_location loc @@ Env.R.lookup gamma idr in
      Result.ok_if_true
        ([%equal: Type.t] typ ty)
        ~error:(Location.locate ~loc:loc
                @@ `TypeMismatchError "Unexpected regular variable type")
  | VarM { idm } ->
      let%bind ty = with_error_location loc @@ Env.M.lookup delta idm in
      Result.ok_if_true
        ([%equal: Type.t] typ ty)
        ~error:(Location.locate ~loc:loc
                @@ `TypeMismatchError "Unexpected modal variable type")
  | Fun { idr; ty_id; body } -> (
      match typ with
      | Type.Arr { dom; cod } ->
          if [%equal: Type.t] dom ty_id then
            check_open delta (Env.R.extend gamma idr dom) body cod
          else
            fail_in loc
            @@ `TypeMismatchError "Domain of arrow type is not the same as type of function \
                               parameter"
      | _ -> fail_in loc
          @@ `TypeMismatchError "Arrow type expected")
  | App { fe; arge } -> (
      let%bind ty = infer_open delta gamma fe in
      match ty with
      | Type.Arr { dom; cod } ->
          let%bind () = check_open delta gamma arge dom in
          Result.ok_if_true
            ([%equal: Type.t] typ cod)
            ~error:(Location.locate ~loc:loc
                    @@ `TypeMismatchError "Unexpected function codomain")
      | _ -> fail_in loc
          @@ `TypeMismatchError "Inferred type is not an arrow type")
  | Box { e } -> (
      match typ with
      | Type.Box { ty } -> check_open delta Env.R.emp e ty
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
      let%map ty = infer_open delta Env.R.emp e in
      Type.Box { ty }
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
