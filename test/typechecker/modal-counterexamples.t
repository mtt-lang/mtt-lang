Prawitz's example of failure of the substitution principle
  $ mtt infer -e "λf:B -> []A. λy:B. (λx:[]A. box x) (f y)"
  mtt: Type inference error: x is not found in the environment!
       file name :  Not a file, lines :  0 - 0, column :  32 - 33
  [1]
