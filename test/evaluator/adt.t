Data constructor is a value
  $ mtt eval <<EOF
  > type List = Nil | Cons of (Nat, List);
  > Cons 5 (Cons 4 Nil)
  > EOF
  Cons 5 (Cons 4 Nil)

Simple pattern-matching

  $ mtt eval <<EOF
  > type List = Nil | Cons of (Nat, List);
  > let tail = fun xs : List.
  >   match xs with
  >   | Cons x xs => xs
  >   | Nil => Nil
  > end;
  > tail (Cons 5 (Cons 4 Nil))
  > EOF
  Cons 4 Nil

  $ mtt eval <<EOF
  > type List = Nil | Cons of (Nat, List);
  > let tail = fun xs : List.
  >   match xs with
  >   | Cons x xs => xs
  >   | Nil => Nil
  > end;
  > tail Nil
  > EOF
  Nil

Recursive patterns

  $ mtt eval <<EOF
  > type List = Nil | Cons of (Nat * Nat, List);
  > match Cons <5, 5> Nil with
  > | Cons <x, y> _ => x + y
  > | _ => 0
  > end
  > EOF
  10

  $ mtt eval <<EOF
  > type List = Nil | Cons of (Nat * Nat, List);
  > match Cons <5, 5> Nil with
  > | Cons <x, _> _ => x
  > | _ => 0
  > end
  > EOF
  5

  $ mtt eval <<EOF
  > type List = Nil | Cons of (Nat * Nat, List);
  > match Cons <5, 5> Nil with
  > | Cons <_, 4> _ => 1
  > | _ => 0
  > end
  > EOF
  0

  $ mtt eval <<EOF
  > type Option = None | Some of (Nat);
  > type List = Nil | Cons of (Option, List);
  > match Cons (Some 5) Nil with
  > | Cons None _ => 1
  > | Cons (Some x) _ => x
  > | Nil => 0
  > end
  > EOF
  5
