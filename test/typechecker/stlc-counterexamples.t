Example of incorrect operation with a tuple
  $ mtt infer -e "λx:A. λy:B. fst x"
  mtt: Type inference error: fst is applied to a non-product type
       file name :  Not a file, lines :  0 - 0, column :  12 - 17
  [124]

  $ mtt infer -e "λx:A. λy:B. snd x"
  mtt: Type inference error: snd is applied to a non-product type
       file name :  Not a file, lines :  0 - 0, column :  12 - 17
  [124]

Example of 
  $ mtt infer -e "λf:A -> B. λy:B . λz:B . (fst <f,y>) z"
  mtt: Type inference error: Unexpected regular variable type
       file name :  Not a file, lines :  0 - 0, column :  37 - 38
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
  >   | zero => <0, 0>
  >   | succ m => ()
  >   end
  > in f 0
  > EOF
  mtt: Type inference error: All branches of pattern matching must have the same type
       file name :  Not a file, lines :  2 - 5, column :  2 - 5
  [124]

  $ mtt infer <<EOF
  > let f = fun n: Nat.
  >   match n with
  >   | zero => <0, 0>
  >   | succ m => ()
  >   end
  > in f 42
  > EOF
  mtt: Type inference error: All branches of pattern matching must have the same type
       file name :  Not a file, lines :  2 - 5, column :  2 - 5
  [124]
