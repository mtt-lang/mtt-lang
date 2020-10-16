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
