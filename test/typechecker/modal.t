Modal apply (or axiom K, or Distribution Axiom in modal logic)
  $ mtt infer -e "(λx:[](A -> B). λy:[]A. letbox u' = x in letbox w' = y in box (u' w'))"
  (□(A → B) → (□A → □B))

Modal axiom T
  $ mtt infer -e "λx:[]A. letbox u' = x in u'"
  (□A → A)

Modal quote
  $ mtt infer -e "λx:[]A. letbox u' = x in box (box u')"
  (□A → □□A)

  $ mtt infer -e "λx:[][]A. letbox u' = x in u'"  
  (□□A → □A)

Simple properties
  $ mtt infer -e "λx:[]A. λy: []B. x"
  (□A → (□B → □A))

  $ mtt infer -e "λx:[]A. letbox u' = x in box (λy:B. u')"
  (□A → □(B → A))

Modal axioms
  $ mtt infer -e "λx:[](A -> B). letbox u' = x in box (λy:[]A. letbox w' = y in box (u' w')) "
  (□(A → B) → □(□A → □B))

  $ mtt infer <<EOF
  > λp:(([](A -> B)) * ([](B -> C))).
  > letbox u' = (fst p) in
  > letbox w' = (snd p) in
  > box (λx:A. w' (u' x))
  > EOF
  ((□(A → B)×□(B → C)) → □(A → C))

Exercises from 15-816 Modal Logic, Andre Platzer
  $ mtt infer <<EOF
  > λp:[](A * B).
  > letbox u' = p in
  > <(fst u'), (snd u')>
  > EOF
  (□(A×B) → (A×B))

  $ mtt infer <<EOF
  > λp:(([]A) * ([](A -> B))).
  > letbox u' = (fst p) in
  > letbox w' = (snd p) in
  > box (w' u')
  > EOF
  ((□A×□(A → B)) → □B)

  $ mtt infer <<EOF
  > λf: ((A * B) -> C).
  > λg: (([]A) * ([]B)).
  > letbox u' = (fst g) in
  > letbox w' = (snd g) in
  > f (<u', w'>)
  > EOF
  (((A×B) → C) → ((□A×□B) → C))
