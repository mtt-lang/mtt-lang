Compiler's semantic now is same as `mtt eval`
  $ mtt compile -e "1 + 2"
  3

  $ mtt compile -e "let x = 43 in x - 1"
  42

Same test as for evaluator
  $ mtt eval <<EOF
  > let f = fun n: Nat.
  >   match n with
  >   | zero => <0, 0>
  >   | succ m => <m, n>
  >   end
  > in f 42
  > EOF
  <41, 42>

  $ mtt compile <<EOF
  > let f = fun n: Nat. 2 * n in
  > f 22 - f 2
  > EOF
  40

  $ mtt compile <<EOF
  > let f = fun p: (Nat * Nat). fst p + snd p in
  > f <20, 22>
  > EOF
  42

  $ mtt compile <<EOF
  > let fact = fix (f: Nat -> Nat) n : Nat. 
  > match n with
  > | zero => 1
  > | succ pn => n * f pn
  > end
  > in fact 5
  > EOF
  120

  $ mtt compile <<EOF
  > let power = fix (f : Nat -> Nat -> Nat) a : Nat .
  > λb : Nat.
  >  match b with
  >  | zero => 1
  >  | succ sb => ((f a) sb) * a
  >  end
  > in (power 3) 4
  > EOF
  81

  $ mtt compile <<EOF
  > let fib = fix (f : Nat -> Nat ) n : Nat .
  > match n with 
  > | zero => 0
  > | succ prev => 
  >   match prev with
  >   | zero => 1
  >   | succ prev2 => (f prev) + (f prev2)
  >   end
  > end
  > in fib 12
  > EOF
  144

  $ mtt compile <<EOF
  > let pow_n = fix (f: (Nat -> [](Nat -> Nat))) n : Nat.
  > match n with
  > | zero => box (λb : Nat. 1)
  > | succ pn => letbox pred_pow' = f pn in box (λb : Nat . b * (pred_pow' b))
  > end
  > in 
  > letbox pow' = pow_n 5 in pow' 3
  > EOF
  243

  $ mtt compile <<EOF
  > let x = box 42 in
  > letbox x' = x in
  > let f = (λy : Nat . x') in
  > f 73
  > EOF
  42

