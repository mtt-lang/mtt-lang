Recursive type definition
  $ mtt infer <<EOF
  > type List = Nil | Cons of (Nat * List);
  > Cons
  > EOF
  ℕ×List → List
