Missing type

  $ mtt infer <<EOF
  > type Bool = F | T of (A);
  > ()
  > EOF
  mtt: Type inference error: "A" is not found in the type environment!
       file name :  Not a file, lines :  1 - 1, column :  22 - 23
  [124]

Incorrect usage of data constructor

  $ mtt infer <<EOF
  > type List = Nil | Cons of (Nat, List);
  > Cons Nil
  > EOF
  mtt: Type inference error: Unexpected data constructor type
       file name :  Not a file, lines :  2 - 2, column :  5 - 8
  [124]

Pattern-matching

  $ mtt infer <<EOF
  > match 5 with
  > | 5 => ()
  > | 0 => 0
  > | _ => ()
  > end
  > EOF
  mtt: Type inference error: Expected (), but found Nat type
       file name :  Not a file, lines :  3 - 3, column :  7 - 8
  [124]

  $ mtt infer <<EOF
  > match 5 with
  > | <x, y> => x + y
  > end
  > EOF
  mtt: Type inference error: Expected product type, but found ℕ
       file name :  Not a file, lines :  2 - 2, column :  2 - 8
  [124]

  $ mtt infer <<EOF
  > match <5, ()> with
  > | <x, <z, y>> => x + y
  > end
  > EOF
  mtt: Type inference error: Expected product type, but found ()
       file name :  Not a file, lines :  2 - 2, column :  6 - 12
  [124]

  $ mtt infer <<EOF
  > type F = A | B;
  > type T = C | D;
  > match B with
  > | D => ()
  > end
  > EOF
  mtt: Type inference error: "D" is not found in the data constructors environment!
       file name :  Not a file, lines :  4 - 4, column :  2 - 3
  [124]

  $ mtt infer <<EOF
  > match () with end
  > EOF
  mtt: Type inference error: Type of match-expression with no branches cannot be inferred
       file name :  Not a file, lines :  1 - 1, column :  0 - 17
  [124]

  $ mtt infer <<EOF
  > type List = Nil | Cons of (Nat, List);
  > match Cons 5 Nil with
  > | Cons x => x
  > end
  > EOF
  mtt: Type inference error: Expected 2 patterns but found 1
       file name :  Not a file, lines :  3 - 3, column :  2 - 8
  [124]
