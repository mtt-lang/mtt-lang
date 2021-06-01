open Base
open Malfunction
open Cam
open Mtt

(* Simple implemetnation *)
let rec compile_open (gamma : var Env.R.t) (delta : var Env.M.t)
    Location.{ data = expr; _ } =
  let open Ast.Expr in
  match expr with
  | Unit -> failwith "error unit"
  | Pair { e1; e2 } ->
      let ce1 = compile_open gamma delta e1 in
      let ce2 = compile_open gamma delta e2 in
      Mblock (2, [ ce1; ce2 ])
  | Fst { e } ->
      let ce = compile_open gamma delta e in
      Mfield (0, ce)
  | Snd { e } ->
      let ce = compile_open gamma delta e in
      Mfield (1, ce)
  | Nat { n } ->
      let num = `Int (Nat.to_int n) in
      Mnum num
  | BinOp { op; e1; e2 } -> (
      let lhs = compile_open gamma delta e1 in
      let rhs = compile_open gamma delta e2 in
      (* TODO: trancate sub and zero division *)
      match op with
      | Add -> IntArith.( + ) lhs rhs
      | Sub -> IntArith.( - ) lhs rhs
      | Mul -> IntArith.( * ) lhs rhs
      | Div -> IntArith.( / ) lhs rhs)
  | VarR { idr } -> (
      match Env.R.lookup gamma idr with
      | Ok v -> Mvar v
      | Error _ -> failwith "unknown regular variable")
  | VarM { idm } -> (
      match Env.M.lookup delta idm with
      | Ok v -> Mforce (Mvar v)
      | Error _ -> failwith "unknown modal variable")
  | Fun { idr; ty_id = _; body } ->
      let v = fresh @@ Id.R.to_string idr in
      let bodyc = compile_open (Env.R.extend gamma idr v) delta body in
      Mlambda ([ v ], bodyc)
  | App { fe; arge } ->
      let fec = compile_open gamma delta fe in
      let argec = [ compile_open gamma delta arge ] in
      Mapply (fec, argec)
  | Box { e } -> Mlazy (compile_open gamma delta e)
  | Let { idr; bound; body } ->
      let v = fresh @@ Id.R.to_string idr in
      let boundc = compile_open gamma delta bound in
      let bodyc = compile_open (Env.R.extend gamma idr v) delta body in
      Mlet ([ `Named (v, boundc) ], bodyc)
  | Letbox { idm; boxed; body } ->
      let v = fresh @@ Id.M.to_string idm in
      let boxedc = compile_open gamma delta boxed in
      let bodyc = compile_open gamma (Env.M.extend delta idm v) body in
      Mlet ([ `Named (v, boxedc) ], bodyc)
  | Match { matched; zbranch; pred; sbranch } ->
      let mc = compile_open gamma delta matched in
      let zc = compile_open gamma delta zbranch in
      let v = fresh @@ Id.R.to_string pred in
      let predv = IntArith.( - ) mc (Mnum (`Int 1)) in
      let sbranchc = compile_open (Env.R.extend gamma pred v) delta sbranch in
      let sc = Mlet ([ `Named (v, predv) ], sbranchc) in
      Mswitch (mc, [ ([ `Intrange (0, 0) ], zc); ([ `Deftag ], sc) ])
  | Fix { self; ty_id = _; idr; idr_ty = _; body } ->
      let f = fresh @@ Id.R.to_string self in
      let x = fresh @@ Id.R.to_string idr in
      let extend_ctx = Env.R.extend (Env.R.extend gamma idr x) self f in
      let bodyc = compile_open extend_ctx delta body in
      Mlet ([ `Recursive [ (f, Mlambda ([ x ], bodyc)) ] ], Mvar f)

(* let rec instr_for_var (n : int) =
  if phys_equal n 0 then [ ISnd ] else IFst :: instr_for_var (n - 1) *)

let rec gen_modal_code (* in current environment *) (cur_env : int Env.R.t)
    (* global de brujin index *)
      (global_reg_env : int Env.R.t)
    (* term for current modal variable *)
      (modal_env : Ast.Expr.t' Location.located Env.M.t)
    Location.{ data = expr; _ } =
  let open Ast.Expr in
  match expr with
  | Unit -> [ IQuote { v = VUnit } ]
  | Pair _ -> failwith "non-impl pair"
  | Fst _ -> failwith "non-impl fst"
  | Snd _ -> failwith "non-impl snd"
  | Nat { n } -> [ IQuote { v = VNum { n = Nat.to_int n } } ]
  | BinOp { op; e1; e2 } -> (
      let lhs = gen_modal_code cur_env global_reg_env modal_env e1 in
      let rhs = gen_modal_code cur_env global_reg_env modal_env e2 in
      match op with
      | Add -> [ IPush ] @ lhs @ [ ISwap ] @ rhs @ [ ICons; IPlus ]
      | Sub -> [ IPush ] @ lhs @ [ ISwap ] @ rhs @ [ ICons; IMinus ]
      | Mul -> [ IPush ] @ lhs @ [ ISwap ] @ rhs @ [ ICons; IMul ]
      | Div -> [ IPush ] @ lhs @ [ ISwap ] @ rhs @ [ ICons; IDiv ])
  | VarR { idr } -> (
      match Env.R.lookup cur_env idr with
      | Ok idx -> [ IVar { i = idx } ]
      | Error _ -> (
          match Env.R.lookup global_reg_env idr with
          | Ok idx ->
              (* check *)
              let sh = Env.R.size cur_env in
              [ IVar { i = idx + sh } ]
          | Error _ -> failwith "no such regular variable"))
  | VarM { idm = _ } -> failwith "((("
  | Fun { idr; ty_id = _; body } ->
      let shifted_cur_env = List.map cur_env ~f:(fun (x, y) -> (x, y + 1)) in
      let gen_body =
        gen_modal_code
          (Env.R.extend shifted_cur_env idr 0)
          global_reg_env modal_env body
      in
      [ ICur { prog = gen_body } ]
  | App { fe; arge } ->
      let gen_fe = gen_modal_code cur_env global_reg_env modal_env fe in
      let gen_arge = gen_modal_code cur_env global_reg_env modal_env arge in
      [ IPush ] @ gen_fe @ [ ISwap ] @ gen_arge @ [ ICons; IApp ]
  | Box _ -> failwith "nested box error"
  | Let { idr; bound; body } ->
      let shifted_cur_env = List.map cur_env ~f:(fun (x, y) -> (x, y + 1)) in
      let gen_bound = gen_modal_code cur_env global_reg_env modal_env bound in
      let gen_body =
        gen_modal_code
          (Env.R.extend shifted_cur_env idr 0)
          global_reg_env modal_env body
      in
      [ IPush ] @ gen_bound @ [ ICons ] @ gen_body
  | Letbox _ -> failwith "nested letbox error"
  | Match { matched; zbranch; pred; sbranch } ->
      let gen_matched =
        gen_modal_code cur_env global_reg_env modal_env matched
      in
      let gen_zbranch =
        gen_modal_code cur_env global_reg_env modal_env zbranch
      in
      let sbranch' =
        letc pred (binop Sub matched (nat @@ Nat.of_int 1)) sbranch
      in
      let gen_sbranch =
        gen_modal_code cur_env global_reg_env modal_env sbranch'
      in
      [ IBranch { cond = gen_matched; c1 = gen_zbranch; c2 = gen_sbranch } ]
  | Fix { self; ty_id = _; idr; idr_ty = _; body } ->
      let shifted_omega = List.map cur_env ~f:(fun (x, y) -> (x, y + 2)) in
      let extended_omega =
        Env.R.extend (Env.R.extend shifted_omega self 1) idr 0
      in
      let gen_body =
        gen_modal_code extended_omega global_reg_env modal_env body
      in
      [ ICurRec { prog = gen_body } ]

let rec compile_only_codegen (omega : int Env.R.t)
    (delta : Ast.Expr.t' Location.located Env.M.t) Location.{ data = expr; _ } =
  let open Ast.Expr in
  match expr with
  | Unit -> [ IQuote { v = VUnit } ]
  | Pair { e1; e2 } ->
      let e =
        CamInterpreter.interept [ VUnit ] @@ compile_only_codegen omega delta e1
      in
      let f =
        CamInterpreter.interept [ VUnit ] @@ compile_only_codegen omega delta e2
      in
      [ IQuote { v = VPair { e; f } } ]
  | Fst _ -> failwith "fst"
  | Snd _ -> failwith "snd"
  | Nat { n } -> [ IQuote { v = VNum { n = Nat.to_int n } } ]
  | BinOp { op; e1; e2 } -> (
      let lhs = compile_only_codegen omega delta e1 in
      let rhs = compile_only_codegen omega delta e2 in
      match op with
      | Add -> [ IPush ] @ lhs @ [ ISwap ] @ rhs @ [ ICons; IPlus ]
      | Sub -> [ IPush ] @ lhs @ [ ISwap ] @ rhs @ [ ICons; IMinus ]
      | Mul -> [ IPush ] @ lhs @ [ ISwap ] @ rhs @ [ ICons; IMul ]
      | Div -> [ IPush ] @ lhs @ [ ISwap ] @ rhs @ [ ICons; IDiv ])
  | VarR { idr } -> (
      match Env.R.lookup omega idr with
      | Ok i -> [ IVar { i } ]
      | Error _ -> failwith ("unknown regular variable " ^ Id.R.to_string idr))
  | VarM { idm } -> (
      match Env.M.lookup delta idm with
      | Ok term -> gen_modal_code Env.R.emp omega delta term
      | Error _ -> failwith ("unknown modal variable " ^ Id.M.to_string idm))
  | Fun { idr; ty_id = _; body } ->
      let shifted_omega = List.map omega ~f:(fun (x, y) -> (x, y + 1)) in
      let gen_body =
        compile_only_codegen (Env.R.extend shifted_omega idr 0) delta body
      in
      [ ICur { prog = gen_body } ]
  | App { fe; arge } ->
      let gen_fe = compile_only_codegen omega delta fe in
      let gen_arge = compile_only_codegen omega delta arge in
      [ IPush ] @ gen_fe @ [ ISwap ] @ gen_arge @ [ ICons; IApp ]
  | Box { e } -> compile_only_codegen omega delta e
  | Let { idr; bound; body } ->
      let shifted_omega = List.map omega ~f:(fun (x, y) -> (x, y + 1)) in
      let gen_bound = compile_only_codegen omega delta bound in
      let gen_body =
        compile_only_codegen (Env.R.extend shifted_omega idr 0) delta body
      in
      [ IPush ] @ gen_bound @ [ ICons ] @ gen_body
  | Letbox { idm; boxed; body } ->
      (* let boxed_gen = compile_only_codegen omega delta boxed in *)
      compile_only_codegen omega (Env.M.extend delta idm boxed) body
  | Match { matched; zbranch; pred; sbranch } ->
      let gen_matched = compile_only_codegen omega delta matched in
      let gen_zbranch = compile_only_codegen omega delta zbranch in
      let sbranch' =
        letc pred (binop Sub matched (nat @@ Nat.of_int 1)) sbranch
      in
      let gen_sbranch = compile_only_codegen omega delta sbranch' in
      [ IBranch { cond = gen_matched; c1 = gen_zbranch; c2 = gen_sbranch } ]
  | Fix { self; ty_id = _; idr; idr_ty = _; body } ->
      let shifted_omega = List.map omega ~f:(fun (x, y) -> (x, y + 2)) in
      let extended_omega =
        Env.R.extend (Env.R.extend shifted_omega self 1) idr 0
      in
      let gen_body = compile_only_codegen extended_omega delta body in
      [ ICurRec { prog = gen_body } ]

let compile = compile_only_codegen Env.R.emp Env.M.emp

let compile_simple = compile_open Env.R.emp Env.M.emp

(* VERY UNSAFE!
   The result must be a nat or a pair *)
let rec obj2val obj =
  if Caml.Obj.is_int obj then
    Ast.Val.Nat { n = Mtt.Nat.of_int @@ Caml.Obj.magic obj }
  else
    match Caml.Obj.magic obj with
    | a, b ->
        let va1 = obj2val a in
        let va2 = obj2val b in
        Ast.Val.Pair { v1 = va1; v2 = va2 }
