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

(* TODO: find analog haskell-replicate  *)
let rec instr_for_var (n : int) =
  if phys_equal n 0 then [ Instructions.Snd ]
  else Instructions.Fst :: instr_for_var (n - 1)

let rec codegen (omega : int Env.R.t) (delta : Cam.Instructions.t list Env.M.t)
    Location.{ data = expr; _ } : Cam.Instructions.t list =
  let open Ast.Expr in
  match expr with
  | Unit -> failwith "error unit"
  | Pair _ -> failwith "error pair"
  | Fst _ -> failwith "error fst"
  | Snd _ -> failwith "error snd"
  | Nat { n } -> [ Instructions.Num { n } ]
  | BinOp { op; e1; e2 } -> (
      let v1 = codegen omega delta e1 in
      let v2 = codegen omega delta e2 in
      match op with
      | Add ->
          [ Instructions.Push ] @ v1 @ [ Instructions.Swap ] @ v2
          @ [ Instructions.Cons; Instructions.Plus ]
      | _ -> failwith "unsupported")
  | VarR { idr } -> (
      match Env.R.lookup omega idr with
      | Ok idx -> instr_for_var idx
      | Error _ -> failwith "unknown regular variable")
  | VarM { idm } -> (
      match Env.M.lookup delta idm with
      | Ok code -> code
      | Error _ -> failwith "unknown modal variable")
  | Fun { idr; ty_id = _; body } ->
      (* TODO: think about shifting *)
      let shifted_omega = List.map omega ~f:(fun (x, y) -> (x, y + 1)) in
      let gen_body = codegen (Env.R.extend shifted_omega idr 0) delta body in
      [ Instructions.Cur { prog = gen_body } ]
  | App { fe; arge } ->
      (* TODO: check shifting *)
      let gen_fe = codegen omega delta fe in
      let gen_arge = codegen omega delta arge in
      [ Instructions.Push ] @ gen_fe @ [ Instructions.Swap ] @ gen_arge
      @ [ Instructions.Cons; Instructions.App ]
  | Box { e } -> codegen omega delta e
  | Let _ -> failwith "error let"
  | Letbox _ -> failwith "error letbox"
  | Match _ -> failwith "error match"
  | Fix _ -> failwith "error fix"

let rec compile_with_codegen (gamma : var Env.R.t)
    (delta : Cam.Instructions.t list Env.M.t) Location.{ data = expr; _ } =
  let open Ast.Expr in
  match expr with
  | Unit -> failwith "error unit"
  | Pair { e1; e2 } ->
      let ce1 = compile_with_codegen gamma delta e1 in
      let ce2 = compile_with_codegen gamma delta e2 in
      Mblock (2, [ ce1; ce2 ])
  | Fst { e } ->
      let ce = compile_with_codegen gamma delta e in
      Mfield (0, ce)
  | Snd { e } ->
      let ce = compile_with_codegen gamma delta e in
      Mfield (1, ce)
  | Nat { n } ->
      let num = `Int (Nat.to_int n) in
      Mnum num
  | BinOp { op; e1; e2 } -> (
      let lhs = compile_with_codegen gamma delta e1 in
      let rhs = compile_with_codegen gamma delta e2 in
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
      | Ok code ->
          let n = CamInterpreter.interept [ Cam.Values.VUnit ] code in
          CamInterpreter.val2malfunction n
      | Error _ -> failwith "unknown modal variable")
  | Fun { idr; ty_id = _; body } ->
      let v = fresh @@ Id.R.to_string idr in
      let bodyc = compile_with_codegen (Env.R.extend gamma idr v) delta body in
      Mlambda ([ v ], bodyc)
  | App { fe; arge } ->
      let fec = compile_with_codegen gamma delta fe in
      let argec = [ compile_with_codegen gamma delta arge ] in
      Mapply (fec, argec)
  | Box { e } -> Mlazy (compile_with_codegen gamma delta e)
  | Let { idr; bound; body } ->
      let v = fresh @@ Id.R.to_string idr in
      let boundc = compile_with_codegen gamma delta bound in
      let bodyc = compile_with_codegen (Env.R.extend gamma idr v) delta body in
      Mlet ([ `Named (v, boundc) ], bodyc)
  | Letbox { idm; boxed; body } ->
      let boxed_gen = codegen Env.R.emp delta boxed in
      compile_with_codegen gamma (Env.M.extend delta idm boxed_gen) body
  | Match { matched; zbranch; pred; sbranch } ->
      let mc = compile_with_codegen gamma delta matched in
      let zc = compile_with_codegen gamma delta zbranch in
      let v = fresh @@ Id.R.to_string pred in
      let predv = IntArith.( - ) mc (Mnum (`Int 1)) in
      let sbranchc =
        compile_with_codegen (Env.R.extend gamma pred v) delta sbranch
      in
      let sc = Mlet ([ `Named (v, predv) ], sbranchc) in
      Mswitch (mc, [ ([ `Intrange (0, 0) ], zc); ([ `Deftag ], sc) ])
  | Fix { self; ty_id = _; idr; idr_ty = _; body } ->
      let f = fresh @@ Id.R.to_string self in
      let x = fresh @@ Id.R.to_string idr in
      let extend_ctx = Env.R.extend (Env.R.extend gamma idr x) self f in
      let bodyc = compile_with_codegen extend_ctx delta body in
      Mlet ([ `Recursive [ (f, Mlambda ([ x ], bodyc)) ] ], Mvar f)

(* let rec compile_only_codegen (gamma : Values.t Env.R.t)
    (delta : Cam.Instructions.t list Env.M.t) Location.{ data = expr; _ } =
  let open Ast.Expr in
  let open Values in
  match expr with
  | Unit -> VUnit
  | Pair { e1; e2 } ->
      let e = compile_only_codegen gamma delta e1 in
      let f = compile_only_codegen gamma delta e2 in
      VPair { e; f }
  | Fst _ -> failwith "fst"
  | Snd _ -> failwith "snd"
  | Nat { n } -> VNum { n = Nat.to_int n }
  | BinOp _ -> failwith "binop"
  | VarR { idr } -> (
      match Env.R.lookup gamma idr with
      | Ok v -> v
      | Error _ -> failwith "unknown regular variable")
  | VarM { idm } -> (
      match Env.M.lookup delta idm with
      | Ok v -> CamInterpreter.interept [ VUnit ] v
      | Error _ -> failwith "unknown modal variable")
  | Fun { idr; ty_id = _; body } -> failwith "unsupported"
  | App _ -> failwith "app"
  | Box _ -> failwith "box isn't supported"
  | Let _ -> failwith "let isn't supported"
  | Letbox _ -> failwith "letbox isn't supported"
  | Match _ -> failwith "match isn't supported"
  | Fix _ -> failwith "fix isn't supported" *)

let compile = compile_with_codegen Env.R.emp Env.M.emp

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
