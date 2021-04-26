open Malfunction
open Mtt

let rec compile_open (gamma : var Env.R.t) Location.{ data = expr; _ } =
  let open Ast.Expr in
  match expr with
  | Unit -> failwith "error unit"
  | Pair { e1; e2 } ->
      let ce1 = compile_open gamma e1 in
      let ce2 = compile_open gamma e2 in
      Mblock (2, [ ce1; ce2 ])
  | Fst { e } ->
      let ce = compile_open gamma e in
      Mfield (0, ce)
  | Snd { e } ->
      let ce = compile_open gamma e in
      Mfield (1, ce)
  | Nat { n } ->
      let num = `Int (Nat.to_int n) in
      Mnum num
  | BinOp { op; e1; e2 } -> (
      let lhs = compile_open gamma e1 in
      let rhs = compile_open gamma e2 in
      match op with
      | Add -> IntArith.( + ) lhs rhs
      (* TODO: truncate subtraction *)
      | Sub -> IntArith.( - ) lhs rhs
      | Mul -> IntArith.( * ) lhs rhs
      | Div -> IntArith.( / ) lhs rhs)
  | VarR { idr } -> (
      match Env.R.lookup gamma idr with
      | Ok v -> Mvar v
      | Error _ -> failwith "unknown variable")
  | VarM _ -> failwith "error varM"
  | Fun { idr; ty_id = _; body } ->
      let v = fresh @@ Id.R.to_string idr in
      let bodyc = compile_open (Env.R.extend gamma idr v) body in
      Mlambda ([ v ], bodyc)
  | App { fe; arge } ->
      let fec = compile_open gamma fe in
      let argec = [ compile_open gamma arge ] in
      Mapply (fec, argec)
  | Box { e = _ } -> failwith "error box"
  | Let { idr; bound; body } ->
      let v = fresh @@ Id.R.to_string idr in
      let boundc = compile_open gamma bound in
      let bodyc = compile_open (Env.R.extend gamma idr v) body in
      Mlet ([ `Named (v, boundc) ], bodyc)
      (* let varname = "x" in
         let var = Ident.create varname in
         let compile_open_bound = compile_open (Expr.nat @@ Nat.of_int 1) in
         let compile_open_body = MVar var in         -- working
         let compile_open_body = Mvar (fresh "x") in -- not work
         Mlet ([ `Named (var, compile_open_bound) ], compile_open_body) *)
  | Letbox { idm = _; boxed = _; body = _ } -> failwith "error letbox"
  | Match { matched; zbranch; pred; sbranch } ->
      let mc = compile_open gamma matched in
      let zc = compile_open gamma zbranch in
      let v = fresh @@ Id.R.to_string pred in
      let predv = IntArith.( - ) mc (Mnum (`Int 1)) in
      let sbranchc = compile_open (Env.R.extend gamma pred v) sbranch in
      let sc = Mlet ([ `Named (v, predv) ], sbranchc) in
      Mswitch (mc, [ ([ `Intrange (0, 0) ], zc); ([ `Deftag ], sc) ])

let compile = compile_open Env.R.emp

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
