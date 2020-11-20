Example of incorrect operation with a tuple
  $ mtt infer -e "λx:A. λy:B. fst x"
  mtt: Type inference error: fst is applied to a non-product type
       file name :  Not a file, lines :  0 - 0, column :  12 - 17
  [1]

  $ mtt infer -e "λx:A. λy:B. snd x"
  mtt: Type inference error: snd is applied to a non-product type
       file name :  Not a file, lines :  0 - 0, column :  12 - 17
  [1]

Example of 
  $ mtt infer -e "λf:A -> B. λy:B . λz:B . (fst <f,y>) z"
  mtt: Type inference error: Unexpected regular variable type
       file name :  Not a file, lines :  0 - 0, column :  37 - 38
  [1]

Prawitz's example of failure of the substitution principle
  $ mtt infer -e "λf:B -> []A. λy:B. (λx:[]A. box x) (f y)"
  mtt: Type inference error: Variable x is not found in the regular context!
       file name :  Not a file, lines :  0 - 0, column :  32 - 33
  [1]

