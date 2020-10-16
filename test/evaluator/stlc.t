K I = K*
  $ mtt eval <<EOF
  > <((λa: (A -> A). λb: B. a) (λa: A. a)) (),
  > (λa: A. λb: B. b) ()>
  > EOF
  <λa. a, λb. b>
