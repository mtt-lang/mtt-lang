```sh
$ mtt infer -e "λf:B -> []A. λy:B. (λx:[]A. box x) (f y)"
Type inference error: Variable x is not found in the regular context!
```
