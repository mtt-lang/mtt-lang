open Base
open Malfunction
open Mtt

(* 
let write2file (filename : string) (content : string) =
  Stdio.Out_channel.write_all filename ~data:content

let dump_term (term : Malfunction.t) : string =
  let show_term t =
    match t with
    | Mnum (`Int n) -> "(block (tag 0) " ^ Int.to_string n ^ ")"
    | _ -> failwith "not impl"
  in
  "(module (_" ^ show_term term ^ ")\n(export))" *)

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

let cogen Location.{ data = expr; _ } =
  let open Ast.Expr in
  match expr with
  | Unit -> failwith "error unit"
  | Pair _ -> failwith "error pair"
  | Fst _ -> failwith "error fst"
  | Snd _ -> failwith "error snd"
  | Nat { n = _ } ->
      (* let num = `Int (Nat.to_int n) in
           let _compiled = Mnum num in
           (* let _ = write2file "cogen.mlf" (dump_term compiled) in *)
         in *)
      let _x = Malfunction_compiler.compile_cmx "compiler/Mal.mlf" in
      let _ = Dynlink.loadfile "compiler/Mal.cmx" in
      (* let _ = Malfunction_compiler.link_executable "result" x in *)
      Mnum (`Int (Nat.to_int @@ !FromMal.s))
      (* Mnum (`Int (!Mal.s)) *)
      (* compiled *)
      (* Malfunction_parser.read_expression @@ Lexing.from_string "(+ 42 43)" *)
  | BinOp _ -> failwith "error binop"
  | VarR _ -> failwith "error varr"
  | VarM _ -> failwith "error varm"
  | Fun _ -> failwith "error fun"
  | App _ -> failwith "error app"
  | Box { e = _ } -> failwith "error box"
  | Let _ -> failwith "error let"
  | Letbox _ -> failwith "error letbox"
  | Match _ -> failwith "error match"
  | Fix _ -> failwith "error fix"

(* let compile = compile_open Env.R.emp Env.M.emp *)

let compile_naive = compile_open Env.R.emp Env.M.emp

let compile =
  (* let _ = Caml.Sys.command "ocamlc -opaque -c .mli" in *)
  cogen

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
