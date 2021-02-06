Prawitz's example of failure of the substitution principle
  $ mtt infer -e "λf:B -> []A. λy:B. (λx:[]A. box x) (f y)"
  mtt: Type inference error: regular variable x (bound at 32:33) cannot accessed from boxed expression
       file name :  Not a file, lines :  0 - 0, column :  28 - 33
  [1]
