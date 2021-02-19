Simple properties
  $ mtt infer -e "λx: A. λy: B. x"
  A → B → A

  $ mtt infer -e "λx: (A -> B). λy: (B -> C). λa: A. y (x a)"
  (A → B) → (B → C) → A → C

Properties of conjuction
  $ mtt infer -e "λx: (A * B). < snd x, fst x >"
  A×B → B×A

  $ mtt infer -e "λx: ((A * B) * C). < fst (fst x), <snd (fst x), snd x> >"
  A×B×C → A×(B×C)

A implies (not not A)
  $ mtt infer -e "λx: A. λf: (A -> False). f x"
  A → (A → False) → False

not not not A implies not A
  $ mtt infer <<EOF
  > λnnna: (((A -> False) -> False) -> False). 
  > λa: A. nnna (λna: (A -> False). na a)
  > EOF
  (((A → False) → False) → False) → A → False

Weak form of Pierce's law
  $ mtt infer <<EOF
  > λabaab: (((A -> B) -> A) -> A) -> B.
  > abaab (λaba: (A -> B) -> A.
  > aba (λa: A. 
  > (abaab (λabaa: ((A -> B) -> A). a))))
  > EOF
  ((((A → B) → A) → A) → B) → B

tests for regular 'let .. in' construction
  $ mtt infer <<EOF
  > fun x: A. let y = x in y
  > EOF
  A → A

  $ mtt infer <<EOF
  > fun x: A. let y = x in x
  > EOF
  A → A

  $ mtt infer <<EOF
  > fun x: A.
  > let f = fun x: A. <x, x> in 
  > (f x)
  > EOF
  A → A×A

  $ mtt infer <<EOF
  > fun p: (A * B).
  > let f = fst p in
  > let s = snd p in
  > <s, f>
  > EOF
  A×B → B×A

  $ mtt infer <<EOF
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
  > let x = () in let x = (fun a: A. a) in x
  > EOF
  A → A

tests for location
  $ mtt infer <<EOF
  > let if = λp:A -> B -> C . λt:B . λe:B. (p t) e in if
  > EOF
  mtt: Type inference error: Unexpected regular variable type
       file name :  Not a file, lines :  1 - 1, column :  42 - 43
  [1]

  $ mtt infer <<EOF
  > let true = λx:A. λy:B . x in
  > let false = λx:A . λy:B . y in
  > let if = λp:A -> B -> C . λt:B . λe:B. (p t) e in if
  > EOF
  mtt: Type inference error: Unexpected regular variable type
       file name :  Not a file, lines :  3 - 3, column :  42 - 43
  [1]

Evaluator for Nat
  $ mtt infer <<EOF
  > let predpred = fun n: Nat.
  >   match n with
  >   | zero => 0
  >   | succ m => m - 1
  >   end
  > in predpred 4
  > EOF
  Nat

  $ mtt infer <<EOF
  > let f = fun n: Nat.
  >   match n with
  >   | zero => <0, 0>
  >   | succ m => <m, n>
  >   end
  > in f 0
  > EOF
  Nat×Nat

  $ mtt infer <<EOF
  > let f = fun x:(). 10 in
  > match f () with
  > | zero => 1
  > | succ m => 2
  > end
  > EOF
  Nat
