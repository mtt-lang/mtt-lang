open Base
open Malfunction
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
      (* After translation all modal variables goes to regular context *)
      let idr = Id.R.mk @@ Id.M.to_string idm in
      match Env.R.lookup gamma idr with
      | Ok v -> Mvar v
      | Error _ -> failwith "unknown modal variable")
  | Fun { idr; ty_id = _; body } ->
      let v = fresh @@ Id.R.to_string idr in
      let bodyc = compile_open (Env.R.extend gamma idr v) delta body in
      Mlambda ([ v ], bodyc)
  | App { fe; arge } ->
      let fec = compile_open gamma delta fe in
      let argec = [ compile_open gamma delta arge ] in
      Mapply (fec, argec)
  | Box { e } ->
      (* Very dangerous, it isn't checked that `fresh` really free in e *)
      let x = Id.R.mk "fresh" in
      let translated = func x Ast.Type.Unit e in
      compile_open gamma delta translated
      (* compile_open gamma delta e *)
  | Let { idr; bound; body } ->
      let v = fresh @@ Id.R.to_string idr in
      let boundc = compile_open gamma delta bound in
      let bodyc = compile_open (Env.R.extend gamma idr v) delta body in
      Mlet ([ `Named (v, boundc) ], bodyc)
      (* let varname = "x" in
         let var = Ident.create varname in
         let compile_open_bound = compile_open (Expr.nat @@ Nat.of_int 1) in
         let compile_open_body = MVar var in         -- working
         let compile_open_body = Mvar (fresh "x") in -- not work
         Mlet ([ `Named (var, compile_open_bound) ], compile_open_body) *)
  | Letbox { idm; boxed; body } ->
      let x = Id.R.mk @@ Id.M.to_string idm in
      let ty =
        Ast.Type.Arr { dom = Ast.Type.Unit; cod = Ast.Type.Base { idt = "A" } }
      in
      (* nat (Nat.of_int 0) is `Unit` in this context.
           After translation we get a function: () -> A.
          So, we should apply this to Unit. *)
      let translated = app (func x ty body) (app boxed (nat @@ Nat.of_int 0)) in
      compile_open gamma delta translated
      (* Naive approach:
         let v = fresh @@ Id.M.to_string idm in
         let boxedc = compile_open gamma delta boxed in
         let bodyc = compile_open gamma (Env.M.extend delta idm v) body in
         Mlet ([ `Named (v, boxedc) ], bodyc) *)
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

let compile = compile_open Env.R.emp Env.M.emp

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
