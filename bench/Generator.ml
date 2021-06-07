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

(* let power_fast_100 =
  let power_fast_text_100 =
    "let pow_n = fix (f: (Nat -> (Nat -> Nat))) n : Nat.\n\
    \    match n with\n\
    \    | zero => (λb : Nat. 1)\n\
    \    | succ pn => let pred_pow = f pn in (λb : Nat . b * \
     (pred_pow b))\n\
    \    end\n\
    \  in let pow = pow_n 100 in pow"
  in
  Util.parse_from_e Term (ParserInterface.String power_fast_text_100) *)

let power_fast_100_unit =
  let power_fast_100_unit_text =
    "let pow_n = fix (f: (Nat -> [](Nat -> Nat))) n : Nat.\n\
    \    match n with\n\
    \    | zero => (fun u: (). (λb : Nat. 1))\n\
    \    | succ pn => (fun x: () -> Nat. (fun y: (). fun b: Nat. b * ((x ()) \
     b))) (f pn)\n\
    \    end\n\
    \    in\n\
    \    (fun a: () -> Nat. (a ()) 5) (pow_n 100)"
  in
  Util.parse_from_e Term (ParserInterface.String power_fast_100_unit_text)

let benchmark _name _pwr_desc _ver = failwith "TODO"

(* let open Mtt_compiler in
   let open Result.Let_syntax in
   let%bind pwr = ver in
   let compiled_pwr = Mtt_compiler.Compiler.compile pwr in
   let iter_num = 500000 in
   let tstart = Unix.gettimeofday () in
   for _ = 1 to iter_num do
     let b = 1 + Random.int 10000 in
     let compiled =
       Mtt_compiler.Cam.genidx
         ([ Cam.IPush ] @ compiled_pwr @ [ Cam.ISwap ]
         @ [ Cam.IQuote { v = Cam.VNum { n = b } }; ICons; IApp ])
         []
     in
     let evaled = Ast.Expr.app pwr (Ast.Expr.nat (Nat.of_int b)) in
     if String.equal name "compiler" then
       let _ = CamInterpreter.interept [ Cam.VUnit ] compiled in
       ()
     else
       let _ = Evaluator.eval evaled in
       ()
   done;
   let tend = Unix.gettimeofday () in
   Stdio.print_endline
     (Printf.sprintf "%12s: %.2f secs%!"
        (name ^ "+" ^ pwr_desc)
        (tend -. tstart));
   Ok () *)

let _ =
  (* let _ = benchmark "compiler" "fast" power_fast_100 in *)
  (* let _ = benchmark "compiler" "slow" power_slow_100 in *)
  (* let _ = benchmark "evaluator" "fast" power_fast_100 in *)
  (* let _ = benchmark "evaluator" "slow" power_slow_100 in *)
  Ok ()
