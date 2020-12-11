Note: the evaluator does not typecheck terms, so if you need to call 
a function like (λa: A. a) you can apply it to the unit value ().

K I = K*
  $ mtt eval <<EOF
  > <((λa: (A -> A). λb: B. a) (λa: A. a)) (),
  >  (λa: A. λb: B. b) ()>
  > EOF
  <λa. a, λb. b>

'Let .. in' expression
  $ mtt eval <<EOF
  > let y = () in (fun x: A. x) y
  > EOF
  ()

  $ mtt eval <<EOF
  > let f = fun x: A. < x, x > in f ()
  > EOF
  <(), ()>

  $ mtt eval <<EOF
  > let f = fun p: A * B. < snd p, fst p > in f < (), () >
  > EOF
  <(), ()>

  $ mtt eval <<EOF
  > (fun p: A * B. 
  > let f = fst p in
  > let s = snd p in
  > < s, f > ) < (), () >
  > EOF
  <(), ()>

Shadowing x
  $ mtt eval <<EOF
  > (let x = () in let x = fun a: A. a in x x)
  > EOF
  λa. a

Church numerals
  $ mtt eval <<EOF
  > let n0 = λf:A -> A. λx:A . x in
  > let n1 = λf:A -> A. λx:A . f x in
  > let n2 = λf:A -> A. λx:A . f (f x) in
  > let n3 = λf:A -> A. λx:A . f (f (f x)) in
  > let n4 = λf:A -> A. λx:A . f (f (f (f x))) in
  > 
  > let true = λx:A. λy:B . x in
  > let false = λx:A . λy:B . y in
  > let if = λp:A -> B -> C . λt:B . λe:B. (p t) e in
  > 
  > let pair = λa:A. λb:B. λt:A -> B -> C . (t a) b in
  > let fstt = λp:A -> B -> C. p true in
  > let sndd = λp:A -> B -> C. p false in 
  > 
  > let succ = λn:(A -> A) -> A -> A. λx:A -> A. λy:A. x ((n x) y) in
  > let pred = λn:(A -> A) -> A -> A. λx:A -> A . λy:A. sndd ( ( n (λp:P . ( pair (x (fstt p)) ) (fstt p) ) ) ( (pair y) y ) ) in
  > let minus = λn:(A -> A) -> A -> A. λm:(A -> A) -> A -> A. (m pred) n in 
  > 
  > let not = λb:A -> B -> C . ((if b) false) true in
  > let iszero = λn:(A -> A) -> A -> A. (n (λx:A -> A -> A. false)) true in
  > let and = λn:(A -> A) -> A -> A. λm:(A -> A) -> A -> A. ((if n) m) false in
  > let eq = λn:(A -> A) -> A -> A. λm:(A -> A) -> A -> A. ( and (iszero ( (minus n) m)) ) (iszero ( (minus m) n)) in
  > 
  > let plus = λn:(A -> A) -> A -> A. λm:(A -> A) -> A -> A. λx:A -> A. λy:A. (n x) ((m x) y) in
  > 
  > let mult = λn:(A -> A) -> A -> A. λm:(A -> A) -> A -> A. λx:A -> A. λy:A. (n (m x)) y in
  > 
  > let fact = λf:(A -> A) -> A -> A -> A. λn:(A -> A) -> A -> A. ((if (iszero n)) n1) ((mult n) (f (pred n))) in
  > 
  > let test1 = (eq n3) n2 in
  > let test2 = (eq n2) n3 in
  > let test3 = (eq n2) n2 in
  > let eqtests = (and ((and (not test1)) (not test2))) test3 in
  > let test4 = (eq n3) ((plus n2) n1) in
  > let test5 = (eq n3) ((plus n2) n2) in
  > let plustest = (and test4) (not test5) in
  > let test6 = (eq n2) ((mult n2) n1) in
  > let test7 = (eq n4) ((mult n2) n2) in
  > let multest = (and test6) test7 in
  > let factest = (eq ((fact (λx:A. x)) n2)) n2 in
  > let runtest = (and ((and ((and eqtests) plustest)) multest)) factest in runtest
  > EOF
  λx. λy : B. x

let succ = λn:(A → A) → A → A. λx:A -> A. λy:(A -> A) -> A -> A. x ((n x) y) in
let pred = 
  λn:(A -> A) -> A -> A. 
  λx:A -> A . 
  λy:(A -> A) -> A -> A. 
  sndd ( ( n (λp:(((A → A) → A → A) → ((A → A) → A → A) → (A → A) → A → A) → (A → A) → A → A. ( pair (x (fstt p)) ) (fstt p) ) ) ( (pair y) y ) ) in true