open Base
open Result.Let_syntax
open Ast
open Ast.Expr

type error = string

let rec check_open delta gamma Location.{data = expr; loc} Location.{data = typ; loc = t_loc} =
  match expr with
  | Unit ->
      Result.ok_if_true
        ([%equal: Type.t'] typ Type.Unit)
        ~error:(Location.pp ~msg:"Expected unit type" loc)
  | Pair (e1, e2) -> (
      match typ with
      | Type.Prod (t1, t2) ->
          let%map () = check_open delta gamma e1 t1
          and () = check_open delta gamma e2 t2 in
          ()
      | _ -> Result.fail @@ Location.pp ~msg:"Expected product type" loc )
  | Fst pe -> (
      let%bind t = infer_open delta gamma pe in
      match t.Location.data with
      | Type.Prod (t1, _t2) ->
          Result.ok_if_true
            ([%equal: Type.t'] typ t1.data)
            ~error:
              (Location.pp
                 ~msg:"fst error: inferred type is different from the input one"
                 t1.loc)
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"fst is applied to a non-product type" t.loc )
  | Snd pe -> (
      let%bind t = infer_open delta gamma pe in
      match t.data with
      | Type.Prod (_t1, t2) ->
          Result.ok_if_true
            ([%equal: Type.t'] typ t2.data)
            ~error:
              (Location.pp
                 ~msg:"snd error: inferred type is different from the input one"
                 t2.loc)
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"snd is applied to a non-product type" t.loc )
  | VarL idl ->
      let%bind t = Env.lookup_r gamma idl in
      Result.ok_if_true
        ([%equal: Type.t'] typ t.Location.data)
        ~error:(Location.pp ~msg:"Unexpected regular variable type" t.Location.loc)
  | VarG idg ->
      let%bind t = Env.lookup_m delta idg in
      Result.ok_if_true
        ([%equal: Type.t'] typ t.Location.data)
        ~error:(Location.pp ~msg:"Unexpected modal variable type" t.Location.loc)
  | Fun (idl, t_of_id, body) -> (
      match typ with
      | Type.Arr (dom, cod) ->
          if [%equal: Type.t'] dom.data t_of_id.data then
            check_open delta (Env.extend_r gamma idl dom) body cod
          else
            Result.fail
            @@ Location.pp
                 ~msg:
                   "Domain of arrow type is not the same as type of function \
                    parameter"
                 t_of_id.loc
      | _ -> Result.fail @@ Location.pp ~msg:"Arror type expected" loc )
  | App (fe, arge) -> (
      let%bind t = infer_open delta gamma fe in
      match t.data with
      | Type.Arr (dom, cod) ->
          let%bind () = check_open delta gamma arge dom in
          Result.ok_if_true
            ([%equal: Type.t'] typ cod.data)
            ~error:(Location.pp ~msg:"Unexpected function codomain" cod.loc)
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"Inferred type is not an arrow type" t.loc )
  | Box e -> (
      match typ with
      | Type.Box t -> check_open delta Env.emp_r e t
      | _ -> Result.fail @@ Location.pp ~msg:"Error: unboxed type" loc )
  | Let (idr, binded_e, body) ->
      let%bind ty = infer_open delta gamma binded_e in
      check_open delta (Env.extend_r gamma idr ty) body (Location.locate ~loc:t_loc typ)
  | Letbox (idg, boxed_e, body) -> (
      let%bind t = infer_open delta gamma boxed_e in
      match t.data with
      | Type.Box t -> check_open (Env.extend_m delta idg t) gamma body (Location.locate ~loc:t_loc typ)
      | _ -> Result.fail @@ Location.pp ~msg:"Inferred type is not a box" t.loc
      )

and infer_open delta gamma Location.{data = expr; loc} =
  match expr with
  | Unit -> return (Location.locate ~loc Type.Unit)
  | Pair (e1, e2) ->
      let%map t1 = infer_open delta gamma e1
      and t2 = infer_open delta gamma e2 in
      Location.locate ~loc:t2.loc (Type.Prod (t1, t2))
  | Fst pe -> (
      let%bind t = infer_open delta gamma pe in
      match t.data with
      | Type.Prod (t1, _t2) -> return t1
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"fst is applied to a non-product type" loc )
  | Snd pe -> (
      let%bind t = infer_open delta gamma pe in
      match t.data with
      | Type.Prod (_t1, t2) -> return t2
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"snd is applied to a non-product type" t.loc )
  | VarL idl -> Env.lookup_r gamma idl
  | VarG idg -> Env.lookup_m delta idg
  | Fun (idl, dom, body) ->
      let%map cod = infer_open delta (Env.extend_r gamma idl dom) body in
      Location.locate ~loc (Type.Arr (dom, cod))
  | App (fe, arge) -> (
      let%bind t = infer_open delta gamma fe in
      match t.data with
      | Type.Arr (dom, cod) ->
          let%bind () = check_open delta gamma arge dom in
          return cod
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"Inferred type is not an arrow type" t.loc )
  | Box e ->
      let%map t = infer_open delta Env.emp_r e in
      Location.locate ~loc (Type.Box t)
  | Let (idr, binded_e, body) ->
      let%bind ty = infer_open delta gamma binded_e in
      infer_open delta (Env.extend_r gamma idr ty) body
  | Letbox (idg, boxed_e, body) -> (
      let%bind ty = infer_open delta gamma boxed_e in
      match ty.data with
      | Type.Box t -> infer_open (Env.extend_m delta idg t) gamma body
      | _ -> Result.fail @@ Location.pp ~msg:"Inferred type is not a box" ty.loc
      )

let check expr typ = check_open Env.emp_m Env.emp_r expr typ

let infer expr = infer_open Env.emp_m Env.emp_r expr
