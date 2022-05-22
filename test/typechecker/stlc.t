Simple properties
  $ mtt infer -e "type A; type B; λx: A. λy: B. x"
  A → B → A

  $ mtt infer -e "type A; type B; type C; λx: (A -> B). λy: (B -> C). λa: A. y (x a)"
  (A → B) → (B → C) → A → C

Properties of conjuction
  $ mtt infer -e "type A; type B; λx: (A * B). < snd x, fst x >"
  A×B → B×A

  $ mtt infer -e "type A; type B; type C; λx: ((A * B) * C). < fst (fst x), <snd (fst x), snd x> >"
  A×B×C → A×(B×C)

A implies (not not A)
  $ mtt infer -e "type A; type False; λx: A. λf: (A -> False). f x"
  A → (A → False) → False

not not not A implies not A
  $ mtt infer <<EOF
  > type A; type False;
  > λnnna: (((A -> False) -> False) -> False). 
  > λa: A. nnna (λna: (A -> False). na a)
  > EOF
  (((A → False) → False) → False) → A → False

Weak form of Pierce's law
  $ mtt infer <<EOF
  > type A; type B;
  > λabaab: (((A -> B) -> A) -> A) -> B.
  > abaab (λaba: (A -> B) -> A.
  > aba (λa: A. 
  > (abaab (λabaa: ((A -> B) -> A). a))))
  > EOF
  ((((A → B) → A) → A) → B) → B

tests for regular 'let .. in' construction
  $ mtt infer <<EOF
  > type A;
  > fun x: A. let y = x in y
  > EOF
  A → A

  $ mtt infer <<EOF
  > type A;
  > fun x: A. let y = x in x
  > EOF
  A → A

  $ mtt infer <<EOF
  > type A;
  > fun x: A.
  > let f = fun x: A. <x, x> in 
  > (f x)
  > EOF
  A → A×A

  $ mtt infer <<EOF
  > type A; type B;
  > fun p: (A * B).
  > let f = fst p in
  > let s = snd p in
  > <s, f>
  > EOF
  A×B → B×A

  $ mtt infer <<EOF
  > type A; type B; type C;
  > fun ab_c: ((A * B) * C).
  > let ab = fst ab_c in
  > let a = fst ab in
  > let b = snd ab in
  > let c = snd ab_c in
  > < a, <b, c> >
  > EOF
  A×B×C → A×(B×C)

Weak form of Pierce's law with 'let .. in'-construction 
  $ mtt infer <<EOF
  > type A; type B;
  > fun abaab: (((A -> B) -> A) -> A) -> B.
  >   let abaa = fun aba: (A -> B) -> A.
  >     let ab = fun a: A.
  >       let b = abaab (fun abaa: (A -> B) -> A . a)
  >       in b
  >     in aba ab
  >   in abaab abaa
  > EOF
  ((((A → B) → A) → A) → B) → B

Shadowing x
  $ mtt infer <<EOF
  > type A;
  > let x = () in let x = (fun a: A. a) in x
  > EOF
  A → A

tests for location
  $ mtt infer <<EOF
  > type A; type B; type C;
  > let if = λp:A -> B -> C . λt:B . λe:B. (p t) e in if
  > EOF
  mtt: Type inference error: Unexpected regular variable type
       file name :  Not a file, lines :  2 - 2, column :  42 - 43
  [124]

  $ mtt infer <<EOF
  > type A; type B; type C;
  > 
  > let true = λx:A. λy:B. x;
  > let false = λx:A . λy:B. y;
  > let if = λp:A -> B -> C. λt:B . λe:B. (p t) e;
  > if
  > EOF
  mtt: Type inference error: Unexpected regular variable type
       file name :  Not a file, lines :  5 - 5, column :  41 - 42
  [124]

Evaluator for Nat
  $ mtt infer <<EOF
  > let predpred = fun n: Nat.
  >   match n with
  >   | 0 => 0
  >   | m => m - 2
  >   end
  > in predpred 4
  > EOF
  ℕ

  $ mtt infer <<EOF
  > let f = fun n: Nat.
  >   match n with
  >   | 0 => <0, 0>
  >   | m => <m, n>
  >   end
  > in f 0
  > EOF
  ℕ×ℕ

  $ mtt infer <<EOF
  > let f = fun x:(). 10 in
  > match f () with
  > | 0 => 1
  > | _ => 2
  > end
  > EOF
  ℕ
