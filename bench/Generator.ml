open Base
open Base.Result.Let_syntax
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

let power_slow_100 =
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
  Util.parse_from_e Term (ParserInterface.String power_fast_text_100)

let benchmark name ver exec =
  let%bind pwr_ver = ver in
  let tstart = Unix.gettimeofday () in
  for _ = 1 to 1000000 do
    (* 1 <= b <= 5 *)
    let b = 1 + Random.int 5 in
    let _ = exec @@ Expr.app pwr_ver (Expr.nat (Nat.of_int b)) in
    ()
  done;
  let tend = Unix.gettimeofday () in
  Stdio.print_endline (Printf.sprintf "%12s: %.2f secs%!" name (tend -. tstart));
  Ok ()

let _ =
  let _ = benchmark "Evaluator + slow" power_slow_100 Evaluator.eval in
  let _ = benchmark "Compiler + slow" power_slow_100 Compiler.compile in
  let _ = benchmark "Evaluator + fast" power_fast_100 Evaluator.eval in
  let _ = benchmark "Compiler + fast" power_fast_100 Compiler.compile in
  Ok ()

(*
  power function, on my laptop
  1000000 iterations
  n^100, where 1 <= n <= 5

  Evaluator + slow: 30.60 secs
  Compiler + slow: 0.42 secs
  Evaluator + fast: 27.13 secs
  Compiler + fast: 0.36 secs  
*)

(* 

On my laptop for self-apply:
   100000 iterations
   Evaluator: 16.03 secs
   Compiler: 18.11 secs

   ******

   1000000 iterations
   Evaluator: 156.91 secs
   Compiler: 179.36 secs

----------------------------

On my laptop on fibonacci:
  1000 iterations, n \in [10, 30]:
  Evaluator: 17.28 secs
  Compiler: 25.42 secs

  but 100000 iterations, n \in [10, 20]:
  Evaluator: 7.07 secs
  Compiler: 6.45 secs

  1000000 iterations, n \in [10, 15]:
  Evaluator: 6.32 secs
  Compiler: 4.42 secs

  1000000 iterations, n = 15
  Evaluator: 14.32 secs
  Compiler: 7.50 secs

  100 iterations, n = 30
   Evaluator: 4.20 secs
   Compiler: 14.92 secs
*)
