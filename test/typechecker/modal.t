Modal apply (or axiom K, or Distribution Axiom in modal logic)
  $ mtt infer -e "(λx:[](A -> B). λy:[]A. letbox u' = x in letbox w' = y in box (u' w'))"
  (□(A → B) → (□A → □B))
