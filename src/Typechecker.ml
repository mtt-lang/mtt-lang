open Base
open Result.Let_syntax
open Ast
open Ast.Expr

type error = string

let rec check_open delta gamma Location.{ data = expr; loc }
    Location.{ data = typ; loc = t_loc } =
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
      let%bind Location.{ data = t; loc = t_loc } = infer_open delta gamma pe in
      match t with
      | Type.Prod (Location.{ data = t1; loc = t1_loc }, _t2) ->
          Result.ok_if_true
            ([%equal: Type.t'] typ t1)
            ~error:
              (Location.pp
                 ~msg:"fst error: inferred type is different from the input one"
                 t1_loc)
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"fst is applied to a non-product type" t_loc )
  | Snd pe -> (
      let%bind Location.{ data = t; loc = t_loc } = infer_open delta gamma pe in
      match t with
      | Type.Prod (_t1, Location.{ data = t2; loc = t2_loc }) ->
          Result.ok_if_true
            ([%equal: Type.t'] typ t2)
            ~error:
              (Location.pp
                 ~msg:"snd error: inferred type is different from the input one"
                 t2_loc)
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"snd is applied to a non-product type" t_loc )
  | VarL idl ->
      let%bind Location.{ data = t; loc = t_loc } = Env.lookup_r gamma idl in
      Result.ok_if_true
        ([%equal: Type.t'] typ t)
        ~error:(Location.pp ~msg:"Unexpected regular variable type" t_loc)
  | VarG idg ->
      let%bind Location.{ data = t; loc = t_loc } = Env.lookup_m delta idg in
      Result.ok_if_true
        ([%equal: Type.t'] typ t)
        ~error:(Location.pp ~msg:"Unexpected modal variable type" t_loc)
  | Fun (idl, Location.{ data = t_of_id; loc = t_of_id_loc }, body) -> (
      match typ with
      | Type.Arr ((Location.{ data = d_dom; _ } as dom), cod) ->
          if [%equal: Type.t'] d_dom t_of_id then
            check_open delta (Env.extend_r gamma idl dom) body cod
          else
            Result.fail
            @@ Location.pp
                 ~msg:
                   "Domain of arrow type is not the same as type of function \
                    parameter"
                 t_of_id_loc
      | _ -> Result.fail @@ Location.pp ~msg:"Arror type expected" loc )
  | App (fe, arge) -> (
      let%bind Location.{ data = t; loc = t_loc } = infer_open delta gamma fe in
      match t with
      | Type.Arr (dom, Location.{ data = cod; loc = cod_loc }) ->
          let%bind () = check_open delta gamma arge dom in
          Result.ok_if_true
            ([%equal: Type.t'] typ cod)
            ~error:(Location.pp ~msg:"Unexpected function codomain" cod_loc)
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"Inferred type is not an arrow type" t_loc )
  | Box e -> (
      match typ with
      | Type.Box t -> check_open delta Env.emp_r e t
      | _ -> Result.fail @@ Location.pp ~msg:"Error: unboxed type" loc )
  | Let (idr, binded_e, body) ->
      let%bind ty = infer_open delta gamma binded_e in
      check_open delta
        (Env.extend_r gamma idr ty)
        body
        (Location.locate ~loc:t_loc typ)
  | Letbox (idg, boxed_e, body) -> (
      let%bind Location.{ data = t; loc = t_loc } =
        infer_open delta gamma boxed_e
      in
      match t with
      | Type.Box t ->
          check_open (Env.extend_m delta idg t) gamma body
            (Location.locate ~loc:t_loc typ)
      | _ -> Result.fail @@ Location.pp ~msg:"Inferred type is not a box" t_loc
      )

and infer_open delta gamma Location.{ data = expr; loc } =
  match expr with
  | Unit -> return (Location.locate ~loc Type.Unit)
  | Pair (e1, e2) ->
      let%map t1 = infer_open delta gamma e1
      and (Location.{ data = _; loc = t2_loc } as t2) =
        infer_open delta gamma e2
      in
      Location.locate ~loc:t2_loc (Type.Prod (t1, t2))
  | Fst pe -> (
      let%bind Location.{ data = t; loc = t_loc } = infer_open delta gamma pe in
      match t with
      | Type.Prod (t1, _t2) -> return t1
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"fst is applied to a non-product type" t_loc )
  | Snd pe -> (
      let%bind Location.{ data = t; loc = t_loc } = infer_open delta gamma pe in
      match t with
      | Type.Prod (_t1, t2) -> return t2
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"snd is applied to a non-product type" t_loc )
  | VarL idl -> (
      match Env.lookup_r gamma idl with
      | Ok res -> return res
      | Error msg -> Result.fail @@ Location.pp ~msg loc )
  | VarG idg -> (
      match Env.lookup_m delta idg with
      | Ok res -> return res
      | Error msg -> Result.fail @@ Location.pp ~msg loc )
  | Fun (idl, dom, body) ->
      let%map cod = infer_open delta (Env.extend_r gamma idl dom) body in
      Location.locate ~loc (Type.Arr (dom, cod))
  | App (fe, arge) -> (
      let%bind Location.{ data = t; loc = t_loc } = infer_open delta gamma fe in
      match t with
      | Type.Arr (dom, cod) ->
          let%bind () = check_open delta gamma arge dom in
          return cod
      | _ ->
          Result.fail
          @@ Location.pp ~msg:"Inferred type is not an arrow type" t_loc )
  | Box e ->
      let%map t = infer_open delta Env.emp_r e in
      Location.locate ~loc (Type.Box t)
  | Let (idr, binded_e, body) ->
      let%bind ty = infer_open delta gamma binded_e in
      infer_open delta (Env.extend_r gamma idr ty) body
  | Letbox (idg, boxed_e, body) -> (
      let%bind Location.{ data = ty; loc = ty_loc } =
        infer_open delta gamma boxed_e
      in
      match ty with
      | Type.Box t -> infer_open (Env.extend_m delta idg t) gamma body
      | _ -> Result.fail @@ Location.pp ~msg:"Inferred type is not a box" ty_loc
      )

let check expr typ = check_open Env.emp_m Env.emp_r expr typ

let infer expr = infer_open Env.emp_m Env.emp_r expr
