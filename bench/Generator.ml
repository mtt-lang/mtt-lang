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

let benchmark name exec =
  let x = gen_fib 30 in
  let tstart = Unix.gettimeofday () in
  for _ = 1 to 100 do
    (* let n = 15 + Random.int 5 in *)
    (* let term = gen_fib n in *)
    let term = x in
    let _ = exec term in
    ()
  done;
  let tend = Unix.gettimeofday () in
  Stdio.print_endline (Printf.sprintf "%12s: %.2f secs%!" name (tend -. tstart))

let _ =
  benchmark "Evaluator" Evaluator.eval;
  benchmark "Compiler" Compiler.compile

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
