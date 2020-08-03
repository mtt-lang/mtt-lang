open Base
open Result.Let_syntax
open Ast
open Ast.Expr

type error = string

let rec check_open delta gamma expr typ =
  match expr with
  | Unit ->
      Result.ok_if_true ([%equal: Type.t] typ Type.Unit) ~error:"Expected unit type"
  | Pair (e1, e2) ->
      begin match typ with
      | Type.Prod (t1, t2) ->
          let%map () = check_open delta gamma e1 t1
          and     () = check_open delta gamma e2 t2 in
          ()
      | _ -> Result.fail "Expected product type"
      end
  | Fst pe ->
      let%bind t = infer_open delta gamma pe in
      begin match t with
      | Type.Prod (t1, _t2) ->
          Result.ok_if_true ([%equal: Type.t] typ t1) ~error:"fst error: inferred type is different from the input one"
      | _ -> Result.fail "fst is applied to a non-product type"
      end
  | Snd pe ->
      let%bind t = infer_open delta gamma pe in
      begin match t with
      | Type.Prod (_t1, t2) ->
          Result.ok_if_true ([%equal: Type.t] typ t2) ~error:"snd error: inferred type is different from the input one"
      | _ -> Result.fail "snd is applied to a non-product type"
      end
  | VarL idl ->
      let%bind t = Env.lookup_l gamma idl in
      Result.ok_if_true ([%equal: Type.t] typ t) ~error:"Unexpected local variable type"
  | VarG idg ->
      let%bind t = Env.lookup_g delta idg in
      Result.ok_if_true ([%equal: Type.t] typ t) ~error:"Unexpected global variable type"
  | Fun (idl, t_of_id, body) ->
      begin match typ with
      | Type.Arr (dom, cod) ->
          if [%equal: Type.t] dom t_of_id then
            check_open delta (Env.extend_l gamma idl dom) body cod
          else
            Result.fail "Domain of arrow type is not the same as type of function parameter"
      | _ ->
          Result.fail "Arror type expected"
      end
  | App (fe, arge) ->
      let%bind t = infer_open delta gamma fe in
      begin match t with
      | Type.Arr (dom, cod) ->
          let%bind () = check_open delta gamma arge dom in
          Result.ok_if_true ([%equal: Type.t] typ cod) ~error:"Unexpected function codomain"
      | _ -> Result.fail "Inferred type is not an arrow type"
      end
  | Box e ->
      begin match typ with
      | Type.Box t -> check_open delta Env.emp_l e t
      | _ -> Result.fail "Error: unboxed type"
      end
  | Letbox (idg, boxed_e, body) ->
      let%bind ty = infer_open delta gamma boxed_e in
      begin match ty with
      | Type.Box t -> check_open (Env.extend_g delta idg t) gamma body typ
      | _ -> Result.fail "Inferred type is not a box"
      end
and infer_open delta gamma expr =
  match expr with
  | Unit ->
      return Type.Unit
  | Pair (e1, e2) ->
      let%map t1 = infer_open delta gamma e1
      and     t2 = infer_open delta gamma e2 in
      Type.Prod (t1, t2)
  | Fst pe ->
      let%bind t = infer_open delta gamma pe in
      begin match t with
      | Type.Prod (t1, _t2) -> return t1
      | _ -> Result.fail "fst is applied to a non-product type"
      end
  | Snd pe ->
      let%bind t = infer_open delta gamma pe in
      begin match t with
      | Type.Prod (_t1, t2) -> return t2
      | _ -> Result.fail "snd is applied to a non-product type"
      end
  | VarL idl ->
      Env.lookup_l gamma idl
  | VarG idg ->
      Env.lookup_g delta idg
  | Fun (idl, dom, body) ->
      let%map cod = infer_open delta (Env.extend_l gamma idl dom) body in
      Type.Arr (dom, cod)
  | App (fe, arge) ->
      let%bind t = infer_open delta gamma fe in
      begin match t with
      | Type.Arr (dom, cod) ->
          let%bind () = check_open delta gamma arge dom in
          return cod
      | _ -> Result.fail "Inferred type is not an arrow type"
      end
  | Box e ->
      let%map t = infer_open delta Env.emp_l e in
      Type.Box t
  | Letbox (idg, boxed_e, body) ->
      let%bind ty = infer_open delta gamma boxed_e in
      begin match ty with
      | Type.Box t -> infer_open (Env.extend_g delta idg t) gamma body
      | _ -> Result.fail "Inferred type is not a box"
      end

let check expr typ =
  check_open Env.emp_g Env.emp_l expr typ

let infer expr =
  infer_open Env.emp_g Env.emp_l expr

