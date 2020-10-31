Note: the evaluator does not typecheck terms, so if you need to call 
a function like (λa: A. a) you can apply it to the unit value ().

K I = K*
  $ mtt eval <<EOF
  > <((λa: (A -> A). λb: B. a) (λa: A. a)) (),
  >  (λa: A. λb: B. b) ()>
  > EOF
  <λa. a, λb. b>

