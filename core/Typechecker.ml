open Base
open Result.Let_syntax
open Ast
open Ast.Expr

type error =
  [ `TypeMismatchError of string
  | `UnboundRegularVarInsideBoxError of Location.t * string
  | `DataCtorArgsQuantityMismatch of string
  | `TypeOfEmptyMatchCannotBeInferred
  | Env.error ]

type 'e lerror = ([> error ] as 'e) Location.located

let check_equal ty1 ty2 msg =
  Result.ok_if_true ([%equal: Type.t] ty1 ty2) ~error:(`TypeMismatchError msg)

let with_error_location loc r =
  Result.map_error r ~f:(fun e -> Location.locate ~loc e)

let fail_in loc err = Result.fail @@ Location.locate ~loc @@ err

(** [mk_unbound_regular_var_inside_box_error loc var_name] create
    [`UnboundRegularVarInsideBoxError] with human readable message.
 *)
let mk_unbound_regular_var_inside_box_error box_expr_loc var_loc var =
  let var_name = Id.R.to_string var in
  let msg =
    [%string
      "regular variable \"$(var_name)\" is accessed in box expression \
       ($(Location.pprint_column_range box_expr_loc)) at \
       $(Location.pprint_column_range var_loc)"]
  in
  fail_in var_loc @@ `UnboundRegularVarInsideBoxError (var_loc, msg)

module Envs = struct
  type t = {
    modal : Type.t Env.M.t;
    regular : Type.t Env.R.t;
    types : TypeDecl.t Env.T.t;
    d_ctors : Type.t Env.D.t;
  }

  let extend_regular envs idr ty =
    { envs with regular = Env.R.extend envs.regular idr ty }

  let extend_modal envs idm ty =
    { envs with modal = Env.M.extend envs.modal idm ty }

  let extend_d_ctors envs idd ty =
    { envs with d_ctors = Env.D.extend envs.d_ctors idd ty }

  let extend_types envs idt decl =
    { envs with types = Env.T.extend envs.types idt decl }

  let extend_d_ctors_many envs idd_ty_pairs =
    { envs with d_ctors = Env.D.extend_many envs.d_ctors idd_ty_pairs }

  let emp =
    {
      modal = Env.M.emp;
      regular = Env.R.emp;
      types = Env.T.emp;
      d_ctors = Env.D.emp;
    }
end

let rec check_type types Location.{ data = ty; loc } =
  match ty with
  | Type.Unit -> return @@ ()
  | Type.Nat -> return @@ ()
  | Type.Base { idt } ->
      let%map _ = with_error_location loc @@ Env.T.lookup types idt in
      ()
  | Type.Arr { dom; cod } ->
      let%map () = check_type types dom and () = check_type types cod in
      ()
  | Type.Box { ty } -> check_type types ty
  | Type.Prod { ty1; ty2 } ->
      let%map () = check_type types ty1 and () = check_type types ty2 in
      ()

let rec lookup_d_ctor_in_decl decl idd =
  match decl with
  | DataCtor.{ idd = idd'; fields } :: other ->
      if Id.D.equal idd idd' then return fields
      else lookup_d_ctor_in_decl other idd
  | [] -> Result.fail @@ Env.D.make_error_of_abscence idd

let lookup_d_ctor_in_type envs idt idd =
  let%bind decl = Env.T.lookup Envs.(envs.types) idt in
  lookup_d_ctor_in_decl decl idd

let rec extend_envs_with_pattern envs Location.{ data = pattern; loc } ty =
  let open Pattern in
  let given_ty = PrettyPrinter.Str.of_type ty in
  let Location.{ data = ty'; _ } = ty in
  let fail_cause_type_mismatch expected =
    fail_in loc
    @@ `TypeMismatchError [%string "Expected $expected, but found $given_ty"]
  in

  match pattern with
  | Ignore -> return envs
  | VarR { idr } -> return @@ Envs.extend_regular envs idr ty
  | Pair { sub1; sub2 } -> (
      match ty' with
      | Type.Prod { ty1; ty2 } ->
          let%bind envs_ext = extend_envs_with_pattern envs sub1 ty1 in
          extend_envs_with_pattern envs_ext sub2 ty2
      | _ -> fail_cause_type_mismatch "product type")
  | Nat { n = _ } -> (
      match ty' with
      | Type.Nat -> return envs
      | _ -> fail_cause_type_mismatch "Nat")
  | Unit -> (
      match ty' with
      | Type.Unit -> return envs
      | _ -> fail_cause_type_mismatch "Unit")
  | DCtor { idd; subs } -> (
      match ty' with
      | Type.Base { idt } ->
          let%bind fields =
            with_error_location loc @@ lookup_d_ctor_in_type envs idt idd
          in
          let%bind () =
            Result.ok_if_true
              (List.length subs = List.length fields)
              ~error:
                (let subs_len_str = Int.to_string @@ List.length subs in
                 let fields_len_str = Int.to_string @@ List.length fields in
                 Location.locate ~loc
                 @@ `DataCtorArgsQuantityMismatch
                      [%string
                        "Expected $fields_len_str patterns but found \
                         $subs_len_str"])
          in
          let field_sub_pairs = Caml.List.combine fields subs in
          let f (field_ty, sub) envs_res =
            envs_res >>= fun envs' ->
            extend_envs_with_pattern envs' sub field_ty
          in
          let init = return envs in
          List.fold_right field_sub_pairs ~f ~init
      | _ -> fail_cause_type_mismatch "base type")

let rec check_expr_open Envs.({ modal; regular; types = _; d_ctors } as envs)
    Location.{ data = expr; loc } (Location.{ data = typ'; _ } as typ) =
  match expr with
  | Unit ->
      let exp_ty = PrettyPrinter.Str.of_type typ in
      with_error_location loc
      @@ check_equal typ Type.unit
           [%string "Expected $exp_ty, but found Unit type"]
  | Pair { e1; e2 } -> (
      match typ' with
      | Type.Prod { ty1; ty2 } ->
          let%map () = check_expr_open envs e1 ty1
          and () = check_expr_open envs e2 ty2 in
          ()
      | _ ->
          let exp_ty = PrettyPrinter.Str.of_type typ in
          fail_in loc
          @@ `TypeMismatchError
               [%string "Expected $exp_ty, but found product type"])
  | Fst { e } -> (
      let%bind Location.{ data = ty; _ } = infer_expr_open envs e in
      match ty with
      | Type.Prod { ty1; ty2 = _ } ->
          with_error_location loc
          @@ check_equal typ ty1
               "fst error: inferred type is different from the input one"
      | _ ->
          fail_in loc
          @@ `TypeMismatchError "fst is applied to a non-product type")
  | Snd { e } -> (
      let%bind Location.{ data = ty; _ } = infer_expr_open envs e in
      match ty with
      | Type.Prod { ty1 = _; ty2 } ->
          with_error_location loc
          @@ check_equal typ ty2
               "snd error: inferred type is different from the input one"
      | _ ->
          fail_in loc
          @@ `TypeMismatchError "snd is applied to a non-product type")
  | Nat _ ->
      let exp_ty = PrettyPrinter.Str.of_type typ in
      with_error_location loc
      @@ check_equal typ Type.nat
           [%string "Expected $exp_ty, but found Nat type"]
  | BinOp { op = _; e1; e2 } ->
      let%map () = check_expr_open envs e1 Type.nat
      and () = check_expr_open envs e2 Type.nat in
      ()
  | VarR { idr } ->
      let%bind ty = with_error_location loc @@ Env.R.lookup regular idr in
      with_error_location loc
      @@ check_equal typ ty "Unexpected regular variable type"
  | VarM { idm } ->
      let%bind ty = with_error_location loc @@ Env.M.lookup modal idm in
      with_error_location loc
      @@ check_equal typ ty "Unexpected modal variable type"
  | VarD { idd } ->
      let%bind ty = with_error_location loc @@ Env.D.lookup d_ctors idd in
      with_error_location loc
      @@ check_equal typ ty "Unexpected data constructor type"
  | Fun { idr; ty_id; body } -> (
      match typ' with
      | Type.Arr { dom; cod } ->
          let%bind () =
            with_error_location loc
            @@ check_equal dom ty_id
                 "Domain of arrow type is not the same as type of function \
                  parameter"
          in
          let envs_ext = Envs.extend_regular envs idr dom in
          check_expr_open envs_ext body cod
      | _ -> fail_in loc @@ `TypeMismatchError "Arrow type expected")
  | App { fe; arge } -> (
      let%bind Location.{ data = ty; _ } = infer_expr_open envs fe in
      match ty with
      | Type.Arr { dom; cod } ->
          let%bind () = check_expr_open envs arge dom in
          with_error_location loc
          @@ check_equal typ cod "Unexpected function codomain"
      | _ ->
          fail_in loc @@ `TypeMismatchError "Inferred type is not an arrow type"
      )
  | Box { e } -> (
      match typ' with
      | Type.Box { ty } -> (
          match check_expr_open { envs with Envs.regular = Env.R.emp } e ty with
          | Error { data = `EnvUnboundRegularVarError (var, _); loc = var_loc }
            when Result.is_ok (Env.R.lookup regular var) ->
              mk_unbound_regular_var_inside_box_error loc var_loc var
          | x -> x)
      | _ -> fail_in loc @@ `TypeMismatchError "Error: unboxed type")
  | Let { idr; bound; body } ->
      let%bind ty = infer_expr_open envs bound in
      let envs_ext = Envs.extend_regular envs idr ty in
      check_expr_open envs_ext body typ
  | Letbox { idm; boxed; body } -> (
      let%bind Location.{ data = ty; _ } = infer_expr_open envs boxed in
      match ty with
      | Type.Box { ty } ->
          let envs_ext = Envs.extend_modal envs idm ty in
          check_expr_open envs_ext body typ
      | _ -> fail_in loc @@ `TypeMismatchError "Inferred type is not a box")
  | Match { matched; branches } ->
      let%bind m_ty = infer_expr_open envs matched in
      let check_branch (pattern, body) =
        let%bind envs_ext = extend_envs_with_pattern envs pattern m_ty in
        check_expr_open envs_ext body typ
      in
      Result.all_unit @@ List.map ~f:check_branch branches

and infer_expr_open Envs.({ modal; regular; types; d_ctors } as envs)
    Location.{ data = expr; loc } : (Type.t, 'e lerror) Result.t =
  match expr with
  | Unit -> return Type.unit
  | Pair { e1; e2 } ->
      let%map ty1 = infer_expr_open envs e1 and ty2 = infer_expr_open envs e2 in
      Type.prod ty1 ty2
  | Fst { e } -> (
      let%bind Location.{ data = ty; _ } = infer_expr_open envs e in
      match ty with
      | Type.Prod { ty1; ty2 = _ } -> return ty1
      | _ ->
          fail_in loc
          @@ `TypeMismatchError "fst is applied to a non-product type")
  | Snd { e } -> (
      let%bind Location.{ data = ty; _ } = infer_expr_open envs e in
      match ty with
      | Type.Prod { ty1 = _; ty2 } -> return ty2
      | _ ->
          fail_in loc
          @@ `TypeMismatchError "snd is applied to a non-product type")
  | Nat _ -> return Type.nat
  | BinOp { op = _; e1; e2 } ->
      let%map () = check_expr_open envs e1 Type.nat
      and () = check_expr_open envs e2 Type.nat in
      Type.nat
  | VarR { idr } -> with_error_location loc @@ Env.R.lookup regular idr
  | VarM { idm } -> with_error_location loc @@ Env.M.lookup modal idm
  | VarD { idd } -> with_error_location loc @@ Env.D.lookup d_ctors idd
  | Fun { idr; ty_id; body } ->
      let%bind () = check_type types ty_id in
      let%map ty_body =
        let envs_ext = Envs.extend_regular envs idr ty_id in
        infer_expr_open envs_ext body
      in
      Type.arr ty_id ty_body
  | App { fe; arge } -> (
      let%bind Location.{ data = ty; _ } = infer_expr_open envs fe in
      match ty with
      | Type.Arr { dom; cod } ->
          let%bind () = check_expr_open envs arge dom in
          return cod
      | _ ->
          fail_in loc @@ `TypeMismatchError "Inferred type is not an arrow type"
      )
  | Box { e } ->
      let%map ty =
        match infer_expr_open { envs with Envs.regular = Env.R.emp } e with
        | Error { data = `EnvUnboundRegularVarError (var, _); loc = var_loc }
          when Result.is_ok @@ Env.R.lookup regular var ->
            mk_unbound_regular_var_inside_box_error loc var_loc var
        | x -> x
      in
      Type.box ty
  | Let { idr; bound; body } ->
      let%bind ty = infer_expr_open envs bound in
      let envs_ext = Envs.extend_regular envs idr ty in
      infer_expr_open envs_ext body
  | Letbox { idm; boxed; body } -> (
      let%bind Location.{ data = tyb; _ } = infer_expr_open envs boxed in
      match tyb with
      | Type.Box { ty } ->
          let envs_ext = Envs.extend_modal envs idm ty in
          infer_expr_open envs_ext body
      | _ -> fail_in loc @@ `TypeMismatchError "Inferred type is not a box")
  | Match { matched; branches } -> (
      let%bind m_ty = infer_expr_open envs matched in
      match branches with
      | first :: other ->
          let infer_branch (pattern, body) =
            let%bind envs_ext = extend_envs_with_pattern envs pattern m_ty in
            infer_expr_open envs_ext body
          in
          let%bind first_typ = infer_branch first in
          let check_branch (pattern, body) =
            let%bind envs_ext = extend_envs_with_pattern envs pattern m_ty in
            check_expr_open envs_ext body first_typ
          in
          let%bind () = Result.all_unit @@ List.map ~f:check_branch other in
          return first_typ
      | [] -> fail_in loc `TypeOfEmptyMatchCannotBeInferred)

(* TODO: Check for data constructor redefinition *)
let data_ctor_type envs idt DataCtor.{ idd = _; fields } :
    (Type.t, 'e lerror) Result.t =
  let%bind () =
    Result.all_unit @@ List.map fields ~f:(check_type Envs.(envs.types))
  in
  let f field_ty ty = Type.arr field_ty ty in
  let init = Type.base idt in
  return @@ List.fold_right ~f ~init fields

(* TODO: Check for type redefinition *)
let decl_type idt decl envs =
  let envs_ext = Envs.extend_types envs idt decl in
  let%bind d_ctor_types =
    Result.all @@ List.map decl ~f:(data_ctor_type envs_ext idt)
  in
  let idds = List.map decl ~f:(fun d_ctor -> DataCtor.(d_ctor.idd)) in
  return
  @@ Envs.extend_d_ctors_many envs_ext
  @@ Stdlib.List.combine idds d_ctor_types

let rec check_prog_open envs Location.{ data = prog; _ } typ =
  match prog with
  | Program.Let { idr; bound; next } ->
      let%bind inferred = infer_expr_open envs bound in
      let envs_ext = Envs.extend_regular envs idr inferred in
      check_prog_open envs_ext next typ
  | Program.Last expr -> check_expr_open envs expr typ
  | Program.Type { idt; decl; next } ->
      let%bind envs_ext = decl_type idt decl envs in
      check_prog_open envs_ext next typ

let rec infer_prog_open envs Location.{ data = prog; _ } =
  match prog with
  | Program.Let { idr; bound; next } ->
      let%bind inferred = infer_expr_open envs bound in
      let envs_ext = Envs.extend_regular envs idr inferred in
      infer_prog_open envs_ext next
  | Program.Last expr -> infer_expr_open envs expr
  | Program.Type { idt; decl; next } ->
      let%bind envs_ext = decl_type idt decl envs in
      infer_prog_open envs_ext next

let check prog typ = check_prog_open Envs.emp prog typ
let infer prog = infer_prog_open Envs.emp prog
