Data constructor is a value
  $ mtt eval <<EOF
  > type List = Nil | Cons of (Nat, List);
  > Cons 5 (Cons 4 Nil)
  > EOF
  Cons 5 (Cons 4 Nil)
