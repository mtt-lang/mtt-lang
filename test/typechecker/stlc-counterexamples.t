Example of incorrect operation with a tuple
  $ mtt infer -e "type A; type B; λx:A. λy:B. fst x"
  mtt: Type inference error: fst is applied to a non-product type
       file name :  Not a file, lines :  0 - 0, column :  28 - 33
  [124]

  $ mtt infer -e "type A; type B; λx:A. λy:B. snd x"
  mtt: Type inference error: snd is applied to a non-product type
       file name :  Not a file, lines :  0 - 0, column :  28 - 33
  [124]

Example of 
  $ mtt infer -e "type A; type B; λf:A -> B. λy:B . λz:B . (fst <f,y>) z"
  mtt: Type inference error: Unexpected regular variable type
       file name :  Not a file, lines :  0 - 0, column :  53 - 54
  [124]

Pattern matching for Nat
  $ mtt infer -e "1 + ()"
  mtt: Type inference error: Expected ℕ, but found Unit type
       file name :  Not a file, lines :  0 - 0, column :  4 - 6
  [124]


  $ mtt infer -e "1 + <1,1>"
  mtt: Type inference error: Expected ℕ, but found product type
       file name :  Not a file, lines :  0 - 0, column :  4 - 9
  [124]

  $ mtt infer -e "<1, 1> + 1"
  mtt: Type inference error: Expected ℕ, but found product type
       file name :  Not a file, lines :  0 - 0, column :  0 - 6
  [124]

  $ mtt infer <<EOF
  > let f = fun n: Nat.
  >   match n with
  >   | 0 => <0, 0>
  >   | m => ()
  >   end
  > in f 0
  > EOF
  mtt: Type inference error: Expected ℕ×ℕ, but found Unit type
       file name :  Not a file, lines :  4 - 4, column :  9 - 11
  [124]

  $ mtt infer <<EOF
  > let f = fun n: Nat.
  >   match n with
  >   | 0 => <0, 0>
  >   | m => ()
  >   end
  > in f 42
  > EOF
  mtt: Type inference error: Expected ℕ×ℕ, but found Unit type
       file name :  Not a file, lines :  4 - 4, column :  9 - 11
  [124]

Unbound type

  $ mtt infer -e "fun x : A. x"
  mtt: Type inference error: "A" is not found in the type environment!
       file name :  Not a file, lines :  0 - 0, column :  8 - 9
  [124]

Recursive functions

  $ mtt infer <<EOF
  > let fib = fix (f : () -> Nat) n : Nat .
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
  mtt: Type inference error: Domain of arrow type is not the same as type of function parameter
       file name :  Not a file, lines :  1 - 1, column :  34 - 37
  [124]
