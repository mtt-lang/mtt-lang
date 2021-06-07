open Base
open Base.Result.Let_syntax
open Cam
open Mtt

type error = [ `CompilationError of string | Env.error ]

let rec gen_bytecode_modal (cur_env : int Env.R.t) (global_env : int Env.R.t)
    (modal_env : Ast.Expr.t' Location.located Env.M.t)
    Location.{ data = expr; _ } =
  let open Ast.Expr in
  match expr with
  | Unit -> [ IQuote { v = VUnit } ]
  | Pair { e1; e2 } ->
      let e =
        CamInterpreter.interept [ VUnit ]
          (gen_bytecode_modal cur_env global_env modal_env e1)
      in
      let f =
        CamInterpreter.interept [ VUnit ]
          (gen_bytecode_modal cur_env global_env modal_env e2)
      in
      [ IQuote { v = VPair { e; f } } ]
  | Fst _ -> failwith "non-impl fst"
  | Snd _ -> failwith "non-impl snd"
  | Nat { n } -> [ IQuote { v = VNum { n = Nat.to_int n } } ]
  | BinOp { op; e1; e2 } -> (
      let lhs = gen_bytecode_modal cur_env global_env modal_env e1 in
      let rhs = gen_bytecode_modal cur_env global_env modal_env e2 in
      match op with
      | Add -> [ IPush ] @ lhs @ [ ISwap ] @ rhs @ [ ICons; IPlus ]
      | Sub -> [ IPush ] @ lhs @ [ ISwap ] @ rhs @ [ ICons; IMinus ]
      | Mul -> [ IPush ] @ lhs @ [ ISwap ] @ rhs @ [ ICons; IMul ]
      | Div -> [ IPush ] @ lhs @ [ ISwap ] @ rhs @ [ ICons; IDiv ])
  | VarR { idr } -> (
      match Env.R.lookup cur_env idr with
      | Ok idx -> [ IVar { i = idx } ]
      | Error _ -> (
          match Env.R.lookup global_env idr with
          | Ok idx ->
              let sh = Env.R.size cur_env in
              [ IVar { i = idx + sh } ]
          | Error _ -> failwith "no such regular variable"))
  | VarM { idm = _ } -> failwith "nested box isn't allowed"
  | Fun { idr; ty_id = _; body } ->
      let shifted_cur_env = List.map cur_env ~f:(fun (x, y) -> (x, y + 1)) in
      let gen_body =
        gen_bytecode_modal
          (Env.R.extend shifted_cur_env idr 0)
          global_env modal_env body
      in
      [ ICur { prog = gen_body } ]
  | App { fe; arge } ->
      let gen_fe = gen_bytecode_modal cur_env global_env modal_env fe in
      let gen_arge = gen_bytecode_modal cur_env global_env modal_env arge in
      [ IPush ] @ gen_fe @ [ ISwap ] @ gen_arge @ [ ICons; IApp ]
  | Box _ -> failwith "nested box isn't allowed"
  | Let { idr; bound; body } ->
      let shifted_cur_env = List.map cur_env ~f:(fun (x, y) -> (x, y + 1)) in
      let gen_bound = gen_bytecode_modal cur_env global_env modal_env bound in
      let gen_body =
        gen_bytecode_modal
          (Env.R.extend shifted_cur_env idr 0)
          global_env modal_env body
      in
      [ IPush ] @ gen_bound @ [ ICons ] @ gen_body
  | Letbox _ -> failwith "nested letbox error"
  | Match { matched; zbranch; pred; sbranch } ->
      let gen_matched =
        gen_bytecode_modal cur_env global_env modal_env matched
      in
      let gen_zbranch =
        gen_bytecode_modal cur_env global_env modal_env zbranch
      in
      let sbranch' =
        letc pred (binop Sub matched (nat @@ Nat.of_int 1)) sbranch
      in
      let gen_sbranch =
        gen_bytecode_modal cur_env global_env modal_env sbranch'
      in
      [ IBranch { cond = gen_matched; c1 = gen_zbranch; c2 = gen_sbranch } ]
  | Fix { self; ty_id = _; idr; idr_ty = _; body } ->
      let shifted_omega = List.map cur_env ~f:(fun (x, y) -> (x, y + 2)) in
      let extended_omega =
        Env.R.extend (Env.R.extend shifted_omega self 1) idr 0
      in
      let gen_body =
        gen_bytecode_modal extended_omega global_env modal_env body
      in
      [ ICurRec { prog = gen_body } ]

let rec compile2CAM (omega : int Env.R.t)
    (delta : Ast.Expr.t' Location.located Env.M.t) Location.{ data = expr; _ } =
  let open Ast.Expr in
  match expr with
  | Unit -> return [ IQuote { v = VUnit } ]
  | Pair { e1; e2 } ->
      (* TODO: remove interept call *)
      let%bind e =
        Result.map
          ~f:(CamInterpreter.interept [ VUnit ])
          (compile2CAM omega delta e1)
      in
      let%bind f =
        Result.map
          ~f:(CamInterpreter.interept [ VUnit ])
          (compile2CAM omega delta e2)
      in
      return @@ [ IQuote { v = VPair { e; f } } ]
  | Fst _ -> failwith "fst"
  | Snd _ -> failwith "snd"
  | Nat { n } -> return [ IQuote { v = VNum { n = Nat.to_int n } } ]
  | BinOp { op; e1; e2 } -> (
      let%bind lhs = compile2CAM omega delta e1 in
      let%bind rhs = compile2CAM omega delta e2 in
      return
      @@
      match op with
      | Add -> [ IPush ] @ lhs @ [ ISwap ] @ rhs @ [ ICons; IPlus ]
      | Sub -> [ IPush ] @ lhs @ [ ISwap ] @ rhs @ [ ICons; IMinus ]
      | Mul -> [ IPush ] @ lhs @ [ ISwap ] @ rhs @ [ ICons; IMul ]
      | Div -> [ IPush ] @ lhs @ [ ISwap ] @ rhs @ [ ICons; IDiv ])
  | VarR { idr } -> (
      match Env.R.lookup omega idr with
      | Ok i -> return [ IVar { i } ]
      | Error err_msg -> Result.fail err_msg)
  | VarM { idm } -> (
      match Env.M.lookup delta idm with
      | Ok term -> return @@ gen_bytecode_modal Env.R.emp omega delta term
      | Error _ -> failwith ("unknown modal variable " ^ Id.M.to_string idm))
  | Fun { idr; ty_id = _; body } ->
      let shifted_omega = List.map omega ~f:(fun (x, y) -> (x, y + 1)) in
      let%bind gen_body =
        compile2CAM (Env.R.extend shifted_omega idr 0) delta body
      in
      return [ ICur { prog = gen_body } ]
  | App { fe; arge } ->
      let%bind gen_fe = compile2CAM omega delta fe in
      let%bind gen_arge = compile2CAM omega delta arge in
      return @@ [ IPush ] @ gen_fe @ [ ISwap ] @ gen_arge @ [ ICons; IApp ]
  | Box { e } -> compile2CAM omega delta e
  | Let { idr; bound; body } ->
      let shifted_omega = List.map omega ~f:(fun (x, y) -> (x, y + 1)) in
      let%bind gen_bound = compile2CAM omega delta bound in
      let%bind gen_body =
        compile2CAM (Env.R.extend shifted_omega idr 0) delta body
      in
      return @@ [ IPush ] @ gen_bound @ [ ICons ] @ gen_body
  | Letbox { idm; boxed; body } ->
      compile2CAM omega (Env.M.extend delta idm boxed) body
  | Match { matched; zbranch; pred; sbranch } ->
      let%bind gen_matched = compile2CAM omega delta matched in
      let%bind gen_zbranch = compile2CAM omega delta zbranch in
      let sbranch' =
        letc pred (binop Sub matched (nat @@ Nat.of_int 1)) sbranch
      in
      let%bind gen_sbranch = compile2CAM omega delta sbranch' in
      return
        [ IBranch { cond = gen_matched; c1 = gen_zbranch; c2 = gen_sbranch } ]
  | Fix { self; ty_id = _; idr; idr_ty = _; body } ->
      let shifted_omega = List.map omega ~f:(fun (x, y) -> (x, y + 2)) in
      let extended_omega =
        Env.R.extend (Env.R.extend shifted_omega self 1) idr 0
      in
      let%bind gen_body = compile2CAM extended_omega delta body in
      return [ ICurRec { prog = gen_body } ]

let compile = compile2CAM Env.R.emp Env.M.emp
