Note: the evaluator does not typecheck terms, so if you need to call 
a function like (λa: A. a) you can apply it to the unit value ().

K I = K*
  $ mtt eval <<EOF
  > type A; type B;
  > <((λa: (A -> A). λb: B. a) (λa: A. a)) (),
  >  (λa: A. λb: B. b) ()>
  > EOF
  <λa. a, λb. b>

'Let .. in' expression
  $ mtt eval <<EOF
  > type A;
  > let y = () in (fun x: A. x) y
  > EOF
  ()

  $ mtt eval <<EOF
  > type A;
  > let f = fun x: A. < x, x > in f ()
  > EOF
  <(), ()>

  $ mtt eval <<EOF
  > type A; type B;
  > let f = fun p: A * B. < snd p, fst p > in f < (), () >
  > EOF
  <(), ()>

  $ mtt eval <<EOF
  > type A; type B;
  > (fun p: A * B. 
  > let f = fst p in
  > let s = snd p in
  > < s, f > ) < (), () >
  > EOF
  <(), ()>

Shadowing x
  $ mtt eval <<EOF
  > type A;
  > (let x = () in let x = fun a: A. a in x x)
  > EOF
  λa. a

  $ mtt eval <<EOF
  > type A;
  > (let x = 5 in fun x: (). x)
  > EOF
  λx. x

Church numerals
  $ mtt eval <<EOF
  > type A; type B; type F; type X; type V;
  > 
  > let n0 = λf:F. λx:X . x;
  > let n1 = λf:F. λx:X . f x;
  > let n2 = λf:F. λx:X . f (f x);
  > let n3 = λf:F. λx:X . f (f (f x));
  > let n4 = λf:F. λx:X . f (f (f (f x)));
  > 
  > let true = λx:A. λy:B . x;
  > let false = λx:A . λy:B . y;
  > let if = λp:P. λt:A. λe:B. (p t) e;
  > 
  > let pair = λa:A . λb:B . λt:A -> B -> C . (t a) b;
  > let fstt = λp:A -> B -> C. p (λx:A. λy:B . x);
  > let sndd = λp:A -> B -> C. p (λx:A . λy:B . y);
  > 
  > let succc = λn:N. λf:F. λx:X. f ((n f) x);
  > let pred = λn:N. λf:F. λx:X. sndd ( ( n (λp:P . ( pair (f (fstt p)) ) (fstt p) ) ) ( (pair x) x ) );
  > let minus = λn:N. λm:N. (m pred) n; 
  > 
  > let not = λb:B . ((if b) false) true;
  > let iszero = λn:N. (n (λx:N. false)) true;
  > let and = λn:N. λm:N. ((if n) m) false;
  > let eq = λn:N. λm:N. ( and (iszero ( (minus n) m)) ) (iszero ( (minus m) n));
  > 
  > let plus = λn:N. λm:N. λf:F. λx:X. (n f) ((m f) x);
  > let mult = λn:N. λm:N. λf:F. λx:X. (n (m f)) x;
  > 
  > let factorial = fix (f : A -> A) n : A . (((if (iszero n)) (λu:(). n1)) (λu:(). (mult n) (f (pred n)))) ();
  > 
  > let test1 = (eq n3) n2;
  > let test2 = (eq n2) n3;
  > let test3 = (eq n2) n2;
  > let eqtests = (and ((and (not test1)) (not test2))) test3;
  > let test4 = (eq n3) ((plus n2) n1);
  > let test5 = (eq n3) ((plus n2) n2);
  > let plustest = (and test4) (not test5);
  > let test6 = (eq n2) ((mult n2) n1);
  > let test7 = (eq n4) ((mult n2) n2);
  > let multest = (and test6) test7;
  > let factest = (eq ((mult n2) n3)) (factorial n3);
  > let runtest = (and ((and ((and eqtests) plustest)) multest)) factest; 
  > runtest
  > EOF
  λx. λy : B. x

test for Nat
  $ mtt eval <<EOF
  > let n = 41 * 42 in n
  > EOF
  1722

  $ mtt eval <<EOF
  > let pred = fun n: Nat. 
  >   match n with
  >   | 0 => 0
  >   | m => m - 1
  >   end
  > in pred 0
  > EOF
  0

  $ mtt eval <<EOF
  > let pred = fun n: Nat. 
  >   match n with
  >   | 0 => 0
  >   | m => m - 1
  >   end
  > in pred 1
  > EOF
  0

  $ mtt eval <<EOF
  > let pred = fun n: Nat. 
  >   match n with
  >   | 0 => 0
  >   | _ => n - 1
  >   end
  > in pred 43
  > EOF
  42

  $ mtt eval <<EOF
  > let f = fun n: Nat.
  >   match n with
  >   | 0 => 1
  >   | m => m + n
  >   end
  > in f 42
  > EOF
  84

  $ mtt eval <<EOF
  > let f = fun n: Nat.
  >   match n with
  >   | 0 => <0, 0>
  >   | m => <m, n>
  >   end
  > in f 0
  > EOF
  <0, 0>

  $ mtt eval <<EOF
  > let f = fun n: Nat.
  >   match n with
  >   | 0 => <0, 0>
  >   | m => <m - 1, n>
  >   end
  > in f 42
  > EOF
  <41, 42>

  $ mtt eval <<EOF
  > type A; type B;
  > 
  > let mkpair = fun a: A. fun b: B. <a, b> in
  > let next = fun n: Nat. n + 1 in
  > let f = fun n: Nat.
  >   match n with
  >   | 0 => ((mkpair 0) (next 1))
  >   | _ => ((mkpair n) (next n))
  >   end
  > in f 100
  > EOF
  <100, 101>

  $ mtt eval <<EOF
  > type A;
  > 
  > let f = fun x: A. 10 in
  > match f () with
  > | 0 => 1
  > | _ => 2
  > end
  > EOF
  2

  $ mtt eval <<EOF
  > type A;
  > 
  > let f = fun x: A. 0 in
  > match f () with
  > | _ => 2
  > | 0 => 1
  > end
  > EOF
  2

  $ mtt eval <<EOF
  > type A;
  > 
  > let f = fun x: A. 5 in
  > match f () with
  > | 3 => 0
  > | x => x
  > | 5 => 2
  > end
  > EOF
  5

  $ mtt eval <<EOF
  > type A;
  > 
  > let f = fun x: A. 5 in
  > match f () with
  > | 3 => 0
  > | 5 => 2
  > | x => x
  > end
  > EOF
  2

  $ mtt eval <<EOF
  > match 1 - 1 with
  > | 0 => 1
  > | _ => 2
  > end
  > EOF
  1

  $ mtt eval <<EOF
  > let n = 1 - 2 in
  > n
  > EOF
  0

Priority tests

  $ mtt eval <<EOF
  > 2 + (2 * 2)
  > EOF
  6

  $ mtt eval <<EOF
  > 2 + 2 * 2
  > EOF
  6

  $ mtt eval <<EOF
  > 2 * 2 + 2
  > EOF
  6

  $ mtt eval <<EOF
  > (2 + 2) * 2
  > EOF
  8

  $ mtt eval <<EOF
  > 2 * (2 + 2)
  > EOF
  8

  $ mtt eval <<EOF
  > let p = <42, 43> in
  > 1 + fst p
  > EOF
  43

  $ mtt eval <<EOF
  > let f = fun n: Nat. n + 1 in
  > 42 - f 1
  > EOF
  40

  $ mtt eval <<EOF
  > let f = fun n: Nat. n + 1 in
  > let g = fun n: Nat. n + 2 in
  > 42 + f (g 2) * 2
  > EOF
  52

  $ mtt eval <<EOF
  > let f = fun n: Nat. 2 * n in
  > f 22 - 2
  > EOF
  42

  $ mtt eval <<EOF
  > let f = fun n: Nat. 2 * n in
  > f 22 - f 2
  > EOF
  40

Recursion Tests

  $ mtt eval <<EOF
  > let fact = fix (f: Nat -> Nat) n : Nat. 
  > match n with
  > | 0 => 1
  > | x => n * f (x - 1)
  > end
  > in fact 5
  > EOF
  120

  $ mtt eval <<EOF
  > let power = fix (f : Nat -> Nat -> Nat) a : Nat .
  > λb : Nat.
  >  match b with
  >  | 0 => 1
  >  | b => (f a (b - 1)) * a
  >  end
  > in (power 3) 4
  > EOF
  81

  $ mtt eval <<EOF
  > let fib = fix (f : Nat -> Nat ) n : Nat .
  > match n with 
  > | 0 => 0
  > | n => 
  >   match n - 1 with
  >   | 0 => 1
  >   | pn => (f pn) + (f (pn - 1))
  >   end
  > end
  > in fib 12
  > EOF
  144

  $ mtt eval <<EOF
  > let pow_n = fix (f: (Nat -> [](Nat -> Nat))) n : Nat.
  > match n with
  > | 0 => box (λb : Nat. 1)
  > | n => letbox pred_pow' = f (n - 1) in box (λb : Nat . b * (pred_pow' b))
  > end
  > in 
  > letbox pow' = pow_n 5 in pow' 3
  > EOF
  243

Bad examples
  $ mtt eval <<EOF
  > let f = fun n: Nat.
  >   match n with
  >   | 0 => <0, 0>
  >   | _ => ()
  >   end
  > in f 0
  > EOF
  <0, 0>

  $ mtt eval <<EOF
  > let f = fun n: Nat.
  >   match n with
  >   | 0 => <0, 0>
  >   | _ => ()
  >   end
  > in f 42
  > EOF
  ()
