Prawitz's example of failure of the substitution principle
  $ mtt infer -e "λf:B -> []A. λy:B. (λx:[]A. box x) (f y)"
  mtt: Type inference error: regular variable "x" is accessed in box expression (28:33) at 32:33
       file name :  Not a file, lines :  0 - 0, column :  32 - 33
  [124]

Error reports about an unknown regular variable in boxed expression in different cases:

find a variable from the context behind a box
  $ mtt infer -e "λx:A. box (λy:B. box (λz:[]A. y))"
  mtt: Type inference error: regular variable "y" is accessed in box expression (17:32) at 30:31
       file name :  Not a file, lines :  0 - 0, column :  30 - 31
  [124]

find a variable from the context behind another box
  $ mtt infer -e "λx:A. box (λy:B. box (λz:[]A. x))"
  mtt: Type inference error: regular variable "x" is accessed in box expression (6:33) at 30:31
       file name :  Not a file, lines :  0 - 0, column :  30 - 31
  [124]

find a variable that isn't contained in any regular context above in the AST
  $ mtt infer -e "λx:A. box (λy:B. box (λz:[]A. w))"
  mtt: Type inference error: "w" is not found in the regular environment!
       file name :  Not a file, lines :  0 - 0, column :  30 - 31
  [124]
