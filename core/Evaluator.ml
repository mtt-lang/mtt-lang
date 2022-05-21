open Base
open Result.Let_syntax
open Ast

type error = [ `EvaluationError of string | Env.error ]

let rec free_vars_m Location.{ data = term; _ } =
  let open Expr in
  match term with
  | Unit -> Set.empty (module Id.M)
  | Pair { e1; e2 } -> Set.union (free_vars_m e1) (free_vars_m e2)
  | Fst { e } | Snd { e } -> free_vars_m e
  | Nat _ -> Set.empty (module Id.M)
  | BinOp { op = _; e1; e2 } -> Set.union (free_vars_m e1) (free_vars_m e2)
  | VarR _ -> Set.empty (module Id.M)
  | VarM { idm } -> Set.singleton (module Id.M) idm
  | VarD _ -> Set.empty (module Id.M)
  | Fun { idr = _; ty_id = _; body } -> free_vars_m body
  | App { fe; arge } -> Set.union (free_vars_m fe) (free_vars_m arge)
  | Box { e } -> free_vars_m e
  | Let { idr = _; bound; body } ->
      Set.union (free_vars_m bound) (free_vars_m body)
  | Letbox { idm; boxed; body } ->
      Set.union (free_vars_m boxed)
        (Set.diff (free_vars_m body) (Set.singleton (module Id.M) idm))
  | Match { matched; branches } ->
      List.fold_right ~init:(free_vars_m matched) ~f:Set.union
      @@ List.map ~f:(fun (_, body) -> free_vars_m body) branches

let refresh_m idm fvs =
  let rec loop (idm : Id.M.t) =
    if Set.mem fvs idm then loop (Id.M.mk (Id.M.to_string idm ^ "'")) else idm
    (* it's fresh enough already :) *)
  in
  if Set.mem fvs idm then Some (loop idm) else None

(* modal (modal) substitution *)
let rec subst_m term identm Location.{ data = body; _ } =
  let open Expr in
  match body with
  | Unit -> Location.locate body
  | Pair { e1; e2 } -> pair (subst_m term identm e1) (subst_m term identm e2)
  | Fst { e } -> fst (subst_m term identm e)
  | Snd { e } -> snd (subst_m term identm e)
  | Nat _n -> Location.locate body
  | BinOp { op; e1; e2 } ->
      binop op (subst_m term identm e1) (subst_m term identm e2)
  | VarR _i -> Location.locate body
  | VarD _i -> Location.locate body
  | VarM { idm } ->
      if [%equal: Id.M.t] identm idm then term else Location.locate body
  | Fun { idr; ty_id; body } -> func idr ty_id (subst_m term identm body)
  | App { fe; arge } -> app (subst_m term identm fe) (subst_m term identm arge)
  | Box { e } -> box (subst_m term identm e)
  | Let { idr; bound; body } ->
      letc idr (subst_m term identm bound) (subst_m term identm body)
  | Letbox { idm; boxed; body } ->
      Location.locate
        (if [%equal: Id.M.t] identm idm then
         Letbox { idm; boxed = subst_m term identm boxed; body }
        else
          match refresh_m idm (free_vars_m term) with
          | Some new_i ->
              let body_with_renamed_bound_var =
                subst_m (var_m new_i) idm body
              in
              Letbox
                {
                  idm = new_i;
                  boxed = subst_m term identm boxed;
                  body = subst_m term identm body_with_renamed_bound_var;
                }
          | None ->
              (* no need to rename the bound var *)
              Letbox
                {
                  idm;
                  boxed = subst_m term identm boxed;
                  body = subst_m term identm body;
                })
  | Match { matched; branches } ->
      match_with (subst_m term identm matched)
      @@ List.map
           ~f:(fun (pattern, body) -> (pattern, subst_m term identm body))
           branches

let rec match_pattern gamma Location.{ data = pattern; _ } (v : Val.t) =
  let open Pattern in
  match pattern with
  | Ignore -> return @@ Some []
  | VarR { idr } -> return @@ Some [ (idr, v) ]
  | Pair { sub1; sub2 } -> (
      match v with
      | Pair { v1; v2 } ->
          let%bind match1 = match_pattern gamma sub1 v1 in
          let%bind match2 = match_pattern gamma sub2 v2 in
          return @@ Option.map ~f:List.concat @@ Option.all [ match1; match2 ]
      | _ -> Result.fail @@ `EvaluationError "Pattern expected pair")
  | DCtor { idd; subs } -> (
      match v with
      | DCtor { idd = idd'; args } ->
          if Id.D.equal idd idd' then
            let%bind _ =
              Result.ok_if_true
                (List.length subs = List.length args)
                ~error:(`EvaluationError "Pattern arguments number mismatch")
            in
            let combined = Caml.List.combine subs args in
            let f (sub, v) = match_pattern gamma sub v in
            let%bind matches = Result.all @@ List.map ~f combined in
            return @@ Option.map ~f:List.concat @@ Option.all matches
          else return None
      | _ -> Result.fail @@ `EvaluationError "Pattern expected data constructor"
      )

module List = struct
  include List

  let rec find_result xs ~f =
    match xs with
    | [] -> Result.return None
    | x :: xs' -> (
        let res = f x in
        match res with
        | Ok opt -> (
            match opt with
            | Some _ -> Result.return opt
            | None -> find_result xs' ~f)
        | Error _ -> res)
end

let rec eval_expr_open gamma Location.{ data = expr; _ } =
  let open Expr in
  match expr with
  | Unit -> return Val.Unit
  | Pair { e1; e2 } ->
      let%map v1 = eval_expr_open gamma e1 and v2 = eval_expr_open gamma e2 in
      Val.Pair { v1; v2 }
  | Fst { e } -> (
      let%bind pv = eval_expr_open gamma e in
      match pv with
      | Val.Pair { v1; v2 = _ } -> return v1
      | _ -> Result.fail @@ `EvaluationError "fst is stuck")
  | Snd { e } -> (
      let%bind pv = eval_expr_open gamma e in
      match pv with
      | Val.Pair { v1 = _; v2 } -> return v2
      | _ -> Result.fail @@ `EvaluationError "snd is stuck")
  | Nat { n } -> return @@ Val.Nat { n }
  | BinOp { op; e1; e2 } -> (
      let%bind lhs = eval_expr_open gamma e1 in
      let%bind rhs = eval_expr_open gamma e2 in
      match (lhs, rhs) with
      | Val.Nat { n = n1 }, Val.Nat { n = n2 } -> (
          match op with
          | Add -> return @@ Val.Nat { n = Nat.add n1 n2 }
          | Sub -> return @@ Val.Nat { n = Nat.sub n1 n2 }
          | Mul -> return @@ Val.Nat { n = Nat.mul n1 n2 }
          | Div ->
              if Nat.equal n2 Nat.zero then
                Result.fail @@ `EvaluationError "Division by zero"
              else return @@ Val.Nat { n = Nat.div n1 n2 })
      | _, _ ->
          Result.fail
          @@ `EvaluationError "Only numbers can be used in arithmetics")
  | VarR { idr } -> Env.R.lookup gamma idr
  | VarM _ ->
      Result.fail
      @@ `EvaluationError
           "Modal variable access is not possible in a well-typed term"
  | VarD { idd } -> return @@ Val.DCtor { idd; args = [] }
  | Fun { idr; ty_id = _; body } ->
      return @@ Val.Clos { idr; body; env = gamma }
  | App { fe; arge } -> (
      let%bind fv = eval_expr_open gamma fe in
      let%bind argv = eval_expr_open gamma arge in
      match fv with
      | Val.Clos { idr; body; env } ->
          eval_expr_open (Env.R.extend env idr argv) body
      | Val.DCtor { idd; args } ->
          return @@ Val.DCtor { idd; args = args @ [ argv ] }
      | _ ->
          Result.fail
          @@ `EvaluationError "Trying to apply an argument to a non-function")
  | Box { e } -> return @@ Val.Box { e }
  | Let { idr; bound; body } ->
      let%bind bound_v = eval_expr_open gamma bound in
      eval_expr_open (Env.R.extend gamma idr bound_v) body
  | Letbox { idm; boxed; body } -> (
      let%bind boxed_v = eval_expr_open gamma boxed in
      match boxed_v with
      | Val.Box { e } -> eval_expr_open gamma (subst_m e idm body)
      | _ ->
          Result.fail @@ `EvaluationError "Trying to unbox a non-box expression"
      )
  | Match { matched; branches } ->
      let%bind v = eval_expr_open gamma matched in
      let try_branch (pattern, body) =
        let%bind new_vars_opt = match_pattern gamma pattern v in
        match new_vars_opt with
        | Some new_vars ->
            let gamma_ext = Env.R.extend_many gamma new_vars in
            let%bind res_v = eval_expr_open gamma_ext body in
            return @@ Some res_v
        | None -> return None
      in
      let%bind res_v = List.find_result ~f:try_branch branches in
      Result.of_option res_v
        ~error:(`EvaluationError "Match expression is not exhaustive")

let rec eval_prog_open gamma Location.{ data = prog; _ } =
  match prog with
  | Program.Let { idr; bound; next } ->
      let%bind bv = eval_expr_open gamma bound in
      let gamma_ext = Env.R.extend gamma idr bv in
      eval_prog_open gamma_ext next
  | Program.Type { idt = _; decl = _; next } -> eval_prog_open gamma next
  | Program.Last expr -> eval_expr_open gamma expr

let eval prog = eval_prog_open Env.R.emp prog
