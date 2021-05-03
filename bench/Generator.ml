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

let benchmark name exec =
  let tstart = Unix.gettimeofday () in
  for _ = 1 to 1000000 do
    (* n \in [1000, 2000] *)
    let n = 1000 + Random.int 1000 in
    let term = gen_self_app n in
    let _ = exec term in
    ()
  done;
  let tend = Unix.gettimeofday () in
  Stdio.print_endline (Printf.sprintf "%12s: %.2f secs%!" name (tend -. tstart))

let _ =
  benchmark "Evaluator" Evaluator.eval;
  benchmark "Compiler" Compiler.compile

(* 

On my laptop:
   100000 iterations
   Evaluator: 16.03 secs
   Compiler: 18.11 secs

   ******

   1000000 iterations
   Evaluator: 156.91 secs
   Compiler: 179.36 secs

*)
