open Malfunction
open Malfunction_interpreter
open Ast

let rec compile_open (gamma : var Env.R.t) Location.{ data = expr; _ } =
  let open Expr in
  match expr with
  | Unit -> failwith "error unit"
  | Pair { e1 = _; e2 = _ } -> failwith "error pair"
  | Fst { e = _ } -> failwith "error first"
  | Snd { e = _ } -> failwith "error second"
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
      | Div -> IntArith.( / ) lhs rhs )
  | VarR { idr } -> (
      match Env.R.lookup gamma idr with
      | Ok v -> Mvar v
      | Error _ -> failwith "unkwnon variable" )
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
  | Match { matched = _; zbranch = _; pred = _; sbranch = _ } ->
      failwith "error match"

let compile = compile_open Env.R.emp

let mval2val mval =
  match mval with
  | Block _ -> failwith "Block isn't implemented yet"
  | Vec _ -> failwith "Vec isn't implemented yet"
  (* malfunction interpreter's representation is very different from mtt's *)
  | Func _ -> failwith "Func isn't implemented yet"
  | Int (_, z) ->
      let n = Nat.of_int @@ Z.to_int z in
      Ast.Val.Nat { n }
  | Float _ -> failwith "float numbers are not supported"
  | Thunk _ -> failwith "thunk is not supported"
