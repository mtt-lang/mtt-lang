Modal apply (or axiom K, or Distribution Axiom in modal logic)
  $ mtt infer -e "type A; type B; (λx:[](A -> B). λy:[]A. letbox u' = x in letbox w' = y in box (u' w'))"
  □(A → B) → □A → □B

Modal axiom T
  $ mtt infer -e "type A; λx:[]A. letbox u' = x in u'"
  □A → A

Modal quote
  $ mtt infer -e "type A; λx:[]A. letbox u' = x in box (box u')"
  □A → □□A

  $ mtt infer -e "type A; λx:[][]A. letbox u' = x in u'"  
  □□A → □A

Simple properties
  $ mtt infer -e "type A; type B; λx:[]A. λy: []B. x"
  □A → □B → □A

  $ mtt infer -e "type A; type B; λx:[]A. letbox u' = x in box (λy:B. u')"
  □A → □(B → A)

Modal axioms
  $ mtt infer -e "type A; type B; λx:[](A -> B). letbox u' = x in box (λy:[]A. letbox w' = y in box (u' w')) "
  □(A → B) → □(□A → □B)

  $ mtt infer <<EOF
  > type A; type B; type C;
  > 
  > λp:(([](A -> B)) * ([](B -> C))).
  > letbox u' = (fst p) in
  > letbox w' = (snd p) in
  > box (λx:A. w' (u' x))
  > EOF
  □(A → B)×□(B → C) → □(A → C)

Exercises from 15-816 Modal Logic, Andre Platzer
  $ mtt infer <<EOF
  > type A; type B;
  > 
  > λp:[](A * B).
  > letbox u' = p in
  > <(fst u'), (snd u')>
  > EOF
  □(A×B) → A×B

  $ mtt infer <<EOF
  > type A; type B;
  > 
  > λp:(([]A) * ([](A -> B))).
  > letbox u' = (fst p) in
  > letbox w' = (snd p) in
  > box (w' u')
  > EOF
  □A×□(A → B) → □B

  $ mtt infer <<EOF
  > type A; type B; type C;
  > 
  > λf: ((A * B) -> C).
  > λg: (([]A) * ([]B)).
  > letbox u' = (fst g) in
  > letbox w' = (snd g) in
  > f (<u', w'>)
  > EOF
  (A×B → C) → □A×□B → C

/examples/boxed-id.mtt
  $ mtt infer <<EOF
  > type A; type B;
  > 
  > (box (λx : A -> B. x))
  > EOF
  □((A → B) → A → B)

/examples/boxed-product-curried.mtt
  $ mtt infer <<EOF
  > type A; type B;
  > 
  > fun x:[]A. fun y:[]B.
  > letbox x' = x in
  > letbox y' = y in
  > box <x', y'>
  > EOF
  □A → □B → □(A×B)

/examples/boxed-product1.mtt
  $ mtt infer <<EOF
  > type A; type B;
  > 
  > fun p:[]A * []B.
  > letbox x' = fst p in
  > letbox y' = snd p in
  > box <x', y'>
  > EOF
  □A×□B → □(A×B)

/examples/boxed-product2.mtt
  $ mtt infer <<EOF
  > type A; type B;
  > 
  > fun p:[](A*B).
  > letbox p' = p in
  > <box (fst p'), box (snd p')>
  > EOF
  □(A×B) → □A×□B

Initial term for testing error reports about an unknown regular variable in boxed expression.
(see ./modal-counterexamples.t)

  $ mtt infer -e "type A; type B; λx:A. box (λy:B. box (λz:[]A. z))"
  A → □(B → □(□A → □A))
