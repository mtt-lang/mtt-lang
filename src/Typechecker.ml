open Base
open Result.Let_syntax
open Ast
open Ast.Expr
open Location

type error = string

let rec check_open delta gamma expr typ =
  match expr.data with
  | Unit ->
      Result.ok_if_true
        ([%equal: Type.t] typ (mkLocation Type.Unit expr.loc))
        ~error:(errorMsg "Expected unit type" expr)
  | Pair (e1, e2) -> (
      match typ.data with
      | Type.Prod (t1, t2) ->
          let%map () = check_open delta gamma e1 t1
          and () = check_open delta gamma e2 t2 in
          ()
      | _ -> Result.fail @@ errorMsg "Expected product type" expr )
  | Fst pe -> (
      let%bind t = infer_open delta gamma pe in
      match t.data with
      | Type.Prod (t1, _t2) ->
          Result.ok_if_true
            ([%equal: Type.t] typ t1)
            ~error:
              (errorMsg
                 "fst error: inferred type is different from the input one" t1)
      | _ -> Result.fail @@ errorMsg "fst is applied to a non-product type" t )
  | Snd pe -> (
      let%bind t = infer_open delta gamma pe in
      match t.data with
      | Type.Prod (_t1, t2) ->
          Result.ok_if_true
            ([%equal: Type.t] typ t2)
            ~error:
              (errorMsg
                 "snd error: inferred type is different from the input one" t2)
      | _ -> Result.fail @@ errorMsg "snd is applied to a non-product type" t )
  | VarL idl ->
      let%bind t = Env.lookup_l gamma idl in
      Result.ok_if_true
        ([%equal: Type.t] typ t)
        ~error:(errorMsg "Unexpected regular variable type" t)
  | VarG idg ->
      let%bind t = Env.lookup_g delta idg in
      Result.ok_if_true
        ([%equal: Type.t] typ t)
        ~error:(errorMsg "Unexpected modal variable type" t)
  | Fun (idl, t_of_id, body) -> (
      match typ.data with
      | Type.Arr (dom, cod) ->
          if [%equal: Type.t] dom t_of_id then
            check_open delta (Env.extend_l gamma idl dom) body cod
          else
            Result.fail
            @@ errorMsg
                 "Domain of arrow type is not the same as type of function \
                  parameter"
                 t_of_id
      | _ -> Result.fail @@ errorMsg "Arror type expected" expr )
  | App (fe, arge) -> (
      let%bind t = infer_open delta gamma fe in
      match t.data with
      | Type.Arr (dom, cod) ->
          let%bind () = check_open delta gamma arge dom in
          Result.ok_if_true
            ([%equal: Type.t] typ cod)
            ~error:(errorMsg "Unexpected function codomain" cod)
      | _ -> Result.fail @@ errorMsg "Inferred type is not an arrow type" t )
  | Box e -> (
      match typ.data with
      | Type.Box t -> check_open delta Env.emp_l e t
      | _ -> Result.fail @@ errorMsg "Error: unboxed type" expr )
  | Letbox (idg, boxed_e, body) -> (
      let%bind t = infer_open delta gamma boxed_e in
      match t.data with
      | Type.Box t -> check_open (Env.extend_g delta idg t) gamma body typ
      | _ -> Result.fail @@ errorMsg "Inferred type is not a box" t )

and infer_open delta gamma expr =
  match expr.data with
  | Unit -> return (mkLocation Type.Unit expr.loc)
  | Pair (e1, e2) ->
      let%map t1 = infer_open delta gamma e1
      and t2 = infer_open delta gamma e2 in
      mkLocation (Type.Prod (t1, t2)) t2.loc
  | Fst pe -> (
      let%bind t = infer_open delta gamma pe in
      match t.data with
      | Type.Prod (t1, _t2) -> return t1
      | _ -> Result.fail @@ errorMsg "fst is applied to a non-product type" t )
  | Snd pe -> (
      let%bind t = infer_open delta gamma pe in
      match t.data with
      | Type.Prod (_t1, t2) -> return t2
      | _ -> Result.fail @@ errorMsg "snd is applied to a non-product type" t )
  | VarL idl -> Env.lookup_l gamma idl
  | VarG idg -> Env.lookup_g delta idg
  | Fun (idl, dom, body) ->
      let%map cod = infer_open delta (Env.extend_l gamma idl dom) body in
      mkLocation (Type.Arr (dom, cod)) expr.loc
  | App (fe, arge) -> (
      let%bind t = infer_open delta gamma fe in
      match t.data with
      | Type.Arr (dom, cod) ->
          let%bind () = check_open delta gamma arge dom in
          return cod
      | _ -> Result.fail @@ errorMsg "Inferred type is not an arrow type" t )
  | Box e ->
      let%map t = infer_open delta Env.emp_l e in
      mkLocation (Type.Box t) expr.loc
  | Letbox (idg, boxed_e, body) -> (
      let%bind t = infer_open delta gamma boxed_e in
      match t.data with
      | Type.Box t -> infer_open (Env.extend_g delta idg t) gamma body
      | _ -> Result.fail @@ errorMsg "Inferred type is not a box" t )

let check expr typ = check_open Env.emp_g Env.emp_l expr typ

let infer expr = infer_open Env.emp_g Env.emp_l expr
