open Base
open Mtt
open Mtt.Ast

let rec gen_self_app (n : int) : Expr.t' Location.located =
  (* types are wrong. It doesn't matter *)
  let termId =
    Expr.func (Id.R.mk "x") (Type.Base { idt = "A" }) (Expr.var_r @@ Id.R.mk "x")
  in
  if phys_equal n 0 then Expr.nat @@ Nat.of_int 42
  else Expr.app termId (gen_self_app @@ (n - 1))

let rec gen_fib (n : int) : Expr.t' Location.located =
  if n <= 2 then Expr.nat @@ Nat.of_int n
  else Expr.binop Expr.Add (gen_fib (n - 1)) (gen_fib (n - 2))

(* let power_slow_100 =
  let power_slow_text_100 =
    "fun x: Nat.\n\
    \  let power = fix (f : Nat -> Nat -> Nat) a : Nat.\n\
     λb : Nat.\n\
    \ match b with\n\
    \ | zero => 1\n\
    \ | succ sb => ((f a) sb) * a\n\
     end in (power x) 100"
  in
  Util.parse_from_e Term (ParserInterface.String power_slow_text_100)

let power_fast_100 =
  let power_fast_text_100 =
    "let pow_n = fix (f: (Nat -> [](Nat -> Nat))) n : Nat.\n\
    \    match n with\n\
    \    | zero => box (λb : Nat. 1)\n\
    \    | succ pn => letbox pred_pow' = f pn in box (λb : Nat . b * \
     (pred_pow' b))\n\
    \    end\n\
    \  in letbox pow' = pow_n 100 in pow'"
  in
  Util.parse_from_e Term (ParserInterface.String power_fast_text_100) *)

let benchmark name =
  let open Mtt_compiler in
  let tstart = Unix.gettimeofday () in
  let compiled = Compiler.compile (gen_fib 20) in
  for _ = 1 to 10000 do
    (* 10 : 30 *)
    let _ = Cam.cam2val @@ CamInterpreter.interept [ Cam.VUnit ] compiled in
    (* let _ = Evaluator.eval (gen_fib 20) in *)
    ()
  done;
  let tend = Unix.gettimeofday () in
  Stdio.print_endline (Printf.sprintf "%12s: %.2f secs%!" name (tend -. tstart))

(* let%bind pwr = ver in
   let tstart = Unix.gettimeofday () in
   for _ = 1 to 1000000 do
     let b = 1 + Random.int 5 in
     let _ = exec @@ Expr.app pwr_ver (Expr.nat (Nat.of_int b)) in
     ()
   done;
   let tend = Unix.gettimeofday () in
   Stdio.print_endline (Printf.sprintf "%12s: %.2f secs%!" name (tend -. tstart));
   Ok () *)

let _ =
  (* let _ = benchmark "Evaluator" gen_fib Evaluator.eval in *)
  let _ = benchmark "compiler" in
  (* "Compiler" gen_fib Mtt_compiler.Compiler.compile in *)
  (* let _ = benchmark "Evaluator + fast" (gen_fib 100) Evaluator.eval in
     let _ =
       benchmark "Compiler + fast" (gen_fib 100) Mtt_compiler.Compiler.compile *)
  (* in *)
  Ok ()
