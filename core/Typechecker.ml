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

  let rec extend_d_ctors_many envs pairs =
    match pairs with
    | (idd, ty) :: other ->
        extend_d_ctors (extend_d_ctors_many envs other) idd ty
    | [] -> envs

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
  | Match { matched; zbranch; pred; sbranch } ->
      let%bind _ = check_expr_open envs matched Type.nat in
      let%bind ty_empty = infer_expr_open envs zbranch in
      let envs_ext = Envs.extend_regular envs pred Type.nat in
      check_expr_open envs_ext sbranch ty_empty

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
  | Match { matched; zbranch; pred; sbranch } ->
      let%bind _ = check_expr_open envs matched Type.nat in
      let%bind ty_zero = infer_expr_open envs zbranch in
      let%bind ty_succ =
        let envs_ext = Envs.extend_regular envs pred Type.nat in
        infer_expr_open envs_ext sbranch
      in
      let%bind () =
        with_error_location loc
        @@ check_equal ty_zero ty_succ
             "All branches of pattern matching must have the same type"
      in
      return ty_zero

let data_ctor_type envs idt DataCtor.{ idd = _; fields } :
    (Type.t, 'e lerror) Result.t =
  let%bind () =
    Result.all_unit @@ List.map fields ~f:(check_type Envs.(envs.types))
  in
  let f field_ty ty = Type.arr field_ty ty in
  let init = Type.base idt in
  return @@ List.fold_right ~f ~init fields

let decl_type idt decl envs =
  let envs_ext = Envs.extend_types envs idt decl in
  let%bind d_ctor_types =
    Result.all @@ List.map decl ~f:(data_ctor_type envs idt)
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
