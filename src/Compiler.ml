open Malfunction
open Ast

let rec compile Location.{ data = expr; _ } =
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
      let lhs = compile e1 in
      let rhs = compile e2 in
      match op with
      | Add -> IntArith.( + ) lhs rhs
      | Sub -> IntArith.( - ) lhs rhs
      | Mul -> IntArith.( * ) lhs rhs
      | Div -> IntArith.( / ) lhs rhs )
  | VarR { idr = _ } -> failwith "error varR"
  | VarM _ -> failwith "error varM"
  | Fun { idr = _; ty_id = _; body = _ } -> failwith "error fun"
  | App { fe = _; arge = _ } -> failwith "error app"
  | Box { e = _ } -> failwith "error box"
  | Let { idr = _; bound = _; body = _ } -> failwith "error let"
  | Letbox { idm = _; boxed = _; body = _ } -> failwith "error letbox"
  | Match { matched = _; zbranch = _; pred = _; sbranch = _ } -> failwith "error match"
