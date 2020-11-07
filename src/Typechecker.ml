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
        ([%equal: Type.t] typ (mkLocated Type.Unit))
        ~error:(pp ~msg:"Expected unit type" expr.loc)
  | Pair (e1, e2) -> (
      match typ.data with
      | Type.Prod (t1, t2) ->
          let%map () = check_open delta gamma e1 t1
          and () = check_open delta gamma e2 t2 in
          ()
      | _ -> Result.fail @@ pp ~msg:"Expected product type" expr.loc )
  | Fst pe -> (
      let%bind t = infer_open delta gamma pe in
      match t.data with
      | Type.Prod (t1, _t2) ->
          Result.ok_if_true
            ([%equal: Type.t] typ t1)
            ~error:
              (pp
              ~msg:"fst error: inferred type is different from the input one" t1.loc)
      | _ -> Result.fail @@ pp ~msg:"fst is applied to a non-product type" t.loc )
  | Snd pe -> (
      let%bind t = infer_open delta gamma pe in
      match t.data with
      | Type.Prod (_t1, t2) ->
          Result.ok_if_true
            ([%equal: Type.t] typ t2)
            ~error:
              (pp
              ~msg:"snd error: inferred type is different from the input one" t2.loc)
      | _ -> Result.fail @@ pp ~msg:"snd is applied to a non-product type" t.loc )
  | VarL idl ->
      let%bind t = Env.lookup_l gamma idl in
      Result.ok_if_true
        ([%equal: Type.t] typ t)
        ~error:(pp ~msg:"Unexpected regular variable type" t.loc)
  | VarG idg ->
      let%bind t = Env.lookup_g delta idg in
      Result.ok_if_true
        ([%equal: Type.t] typ t)
        ~error:(pp ~msg:"Unexpected modal variable type" t.loc)
  | Fun (idl, t_of_id, body) -> (
      match typ.data with
      | Type.Arr (dom, cod) ->
          if [%equal: Type.t] dom t_of_id then
            check_open delta (Env.extend_l gamma idl dom) body cod
          else
            Result.fail
            @@ pp
            ~msg:"Domain of arrow type is not the same as type of function \
                  parameter"
                 t_of_id.loc
      | _ -> Result.fail @@ pp ~msg:"Arror type expected" expr.loc )
  | App (fe, arge) -> (
      let%bind t = infer_open delta gamma fe in
      match t.data with
      | Type.Arr (dom, cod) ->
          let%bind () = check_open delta gamma arge dom in
          Result.ok_if_true
            ([%equal: Type.t] typ cod)
            ~error:(pp ~msg:"Unexpected function codomain" cod.loc)
      | _ -> Result.fail @@ pp ~msg:"Inferred type is not an arrow type" t.loc )
  | Box e -> (
      match typ.data with
      | Type.Box t -> check_open delta Env.emp_l e t
      | _ -> Result.fail @@ pp ~msg:"Error: unboxed type" expr.loc )
  | Letbox (idg, boxed_e, body) -> (
      let%bind t = infer_open delta gamma boxed_e in
      match t.data with
      | Type.Box t -> check_open (Env.extend_g delta idg t) gamma body typ
      | _ -> Result.fail @@ pp ~msg:"Inferred type is not a box" t.loc )

and infer_open delta gamma expr =
  match expr.data with
  | Unit -> return (mkLocated ~loc:expr.loc Type.Unit)
  | Pair (e1, e2) ->
      let%map t1 = infer_open delta gamma e1
      and t2 = infer_open delta gamma e2 in
      mkLocated ~loc:t2.loc (Type.Prod (t1, t2))
  | Fst pe -> (
      let%bind t = infer_open delta gamma pe in
      match t.data with
      | Type.Prod (t1, _t2) -> return t1
      | _ -> Result.fail @@ pp ~msg:"fst is applied to a non-product type" expr.loc )
  | Snd pe -> (
      let%bind t = infer_open delta gamma pe in
      match t.data with
      | Type.Prod (_t1, t2) -> return t2
      | _ -> Result.fail @@ pp ~msg:"snd is applied to a non-product type" t.loc )
  | VarL idl -> Env.lookup_l gamma idl
  | VarG idg -> Env.lookup_g delta idg
  | Fun (idl, dom, body) ->
      let%map cod = infer_open delta (Env.extend_l gamma idl dom) body in
      mkLocated ~loc:expr.loc (Type.Arr (dom, cod))
  | App (fe, arge) -> (
      let%bind t = infer_open delta gamma fe in
      match t.data with
      | Type.Arr (dom, cod) ->
          let%bind () = check_open delta gamma arge dom in
          return cod
      | _ -> Result.fail @@ pp ~msg:"Inferred type is not an arrow type" t.loc )
  | Box e ->
      let%map t = infer_open delta Env.emp_l e in
      mkLocated ~loc:expr.loc (Type.Box t)
  | Letbox (idg, boxed_e, body) -> (
      let%bind t = infer_open delta gamma boxed_e in
      match t.data with
      | Type.Box t -> infer_open (Env.extend_g delta idg t) gamma body
      | _ -> Result.fail @@ pp ~msg:"Inferred type is not a box" t.loc )

let check expr typ = check_open Env.emp_g Env.emp_l expr typ

let infer expr = infer_open Env.emp_g Env.emp_l expr
