Recursive type definition

  $ mtt infer <<EOF
  > type List = Nil | Cons of (Nat, List);
  > Cons 3 (Cons 5 Nil)
  > EOF
  List

Type of data constructor

  $ mtt infer <<EOF 
  > type List = Nil | Cons of (Nat, List);
  > Cons
  > EOF
  ℕ → List → List

Pattern-matching

  $ mtt infer <<EOF
  > match 5 with
  > | 5 => ()
  > | 0 => ()
  > | _ => ()
  > end
  > EOF
  ()

  $ mtt infer <<EOF
  > type List = Nil | Cons of (Nat * Nat, List);
  > match Cons <5, 5> Nil with
  > | Cons <x, y> _ => x + y
  > | _ => 0
  > end
  > EOF
  ℕ

  $ mtt infer <<EOF
  > type List = Nil | Cons of (Nat * Nat, List);
  > match Cons <5, 5> Nil with
  > | Cons <x, _> _ => x
  > | _ => 0
  > end
  > EOF
  ℕ

  $ mtt infer <<EOF
  > type List = Nil | Cons of (Nat * Nat, List);
  > match Cons <5, 5> Nil with
  > | Cons <_, 4> _ => 1
  > | _ => 0
  > end
  > EOF
  ℕ

  $ mtt infer <<EOF
  > type Option = None | Some of (Nat);
  > type List = Nil | Cons of (Option, List);
  > match Cons (Some 5) Nil with
  > | Cons None _ => 1
  > | Cons (Some x) _ => x
  > | Nil => 0
  > end
  > EOF
  ℕ
