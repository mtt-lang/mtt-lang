Simple properties
  $ mtt infer -e "λx: A. λy: B. x"
  (A → (B → A))

  $ mtt infer -e "λx: (A -> B). λy: (B -> C). λa: A. y (x a)"
  ((A → B) → ((B → C) → (A → C)))

Properties of conjuction
  $ mtt infer -e "λx: (A * B). < snd x, fst x >"
  ((A×B) → (B×A))

  $ mtt infer -e "λx: ((A * B) * C). < fst (fst x), <snd (fst x), snd x> >"
  (((A×B)×C) → (A×(B×C)))

A implies (not not A)
  $ mtt infer -e "λx: A. λf: (A -> False). f x"
  (A → ((A → False) → False))

not not not A implies not A
  $ mtt infer <<EOF
  > λnnna: (((A -> False) -> False) -> False). 
  > λa: A. nnna (λna: (A -> False). na a)
  > EOF
  ((((A → False) → False) → False) → (A → False))

Weak form of Pierce's law
  $ mtt infer <<EOF
  > λabaab: (((A -> B) -> A) -> A) -> B.
  > abaab (λaba: (A -> B) -> A.
  > aba (λa: A. 
  > (abaab (λabaa: ((A -> B) -> A). a))))
  > EOF
  (((((A → B) → A) → A) → B) → B)

tests for regular 'let .. in' construction
  $ mtt infer <<EOF
  > fun x: A. let y = x in y
  > EOF
  (A → A)

  $ mtt infer <<EOF
  > fun x: A. let y = x in x
  > EOF
  (A → A)

  $ mtt infer <<EOF
  > fun x: A.
  > let f = fun x: A. <x, x> in 
  > (f x)
  > EOF
  (A → (A×A))

  $ mtt infer <<EOF
  > fun p: (A * B).
  > let f = fst p in
  > let s = snd p in
  > <s, f>
  > EOF
  ((A×B) → (B×A))

  $ mtt infer <<EOF
  > fun x: ((A * B) * C).
  > let fx = fst x in
  > let ffx = fst fx in
  > let sfx = snd fx in
  > let sx = snd x in
  > < ffx, <sfx, sx> >
  > EOF
  (((A×B)×C) → (A×(B×C)))

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
  (((((A → B) → A) → A) → B) → B)
