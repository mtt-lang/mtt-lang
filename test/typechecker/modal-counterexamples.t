Prawitz's example of failure of the substitution principle
  $ mtt infer -e "type A; type B; λf:B -> []A. λy:B. (λx:[]A. box x) (f y)"
  mtt: Type inference error: regular variable "x" is accessed in box expression (44:49) at 48:49
       file name :  Not a file, lines :  0 - 0, column :  48 - 49
  [124]

Error reports about an unknown regular variable in boxed expression in different cases:

find a variable from the context behind a box
  $ mtt infer -e "type A; type B; λx:A. box (λy:B. box (λz:[]A. y))"
  mtt: Type inference error: regular variable "y" is accessed in box expression (33:48) at 46:47
       file name :  Not a file, lines :  0 - 0, column :  46 - 47
  [124]

find a variable from the context behind another box
  $ mtt infer -e "type A; type B; λx:A. box (λy:B. box (λz:[]A. x))"
  mtt: Type inference error: regular variable "x" is accessed in box expression (22:49) at 46:47
       file name :  Not a file, lines :  0 - 0, column :  46 - 47
  [124]

find a variable that isn't contained in any regular context above in the AST
  $ mtt infer -e "type A; type B; λx:A. box (λy:B. box (λz:[]A. w))"
  mtt: Type inference error: "w" is not found in the regular environment!
       file name :  Not a file, lines :  0 - 0, column :  46 - 47
  [124]
