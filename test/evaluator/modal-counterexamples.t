bad accesses the free modal variable y'
  $ mtt eval <<EOF
  > letbox x' =
  > box (let z = y' in z)
  > in
  > letbox y' = box () in
  > x'
  > EOF
  mtt: Evaluation error: Modal variable access is not possible in a well-typed term
  [1]
