# Parser errors

## Misc.
  $ mtt parse -e ""
  mtt: Parse error: An expression is expected. This may result from a missing or unexpected lexeme or an attempt to parse a type-level expression
  [124]

## Keyword-related errors

### Cannot use keywords as identifiers (it's hard to come up with a good error message in this case)
(All of the following error messages can be changed)
  $ mtt parse -e "box box"
  mtt: Parse error: Boxed expression is expected
  [124]
  $ mtt parse -e "f box"
  mtt: Parse error: Sometimes this happens when you say "let box" instead of "letbox" or try to apply a function to "box"
  [124]
  $ mtt parse -e "fst box"
  mtt: Parse error: An expression after fst is expected. This primitive can be only used fully applied.
  [124]
  $ mtt parse -e "snd box"
  mtt: Parse error: An expression after snd is expected. This primitive can be only used fully applied.
  [124]
  $ mtt parse -e "fun box : A. box"
  mtt: Parse error: A regular identifier expected. This happens, when e.g. a modal identifier is used fun x' : T. ...
  [124]


## Types

### Missing type annotations
  $ mtt parse -e "fun x => x"
  mtt: Parse error: Function parameter is missing type annotation
  [124]

### Type idents must be capitalized
  $ mtt parse -e "fun x : a. x"
  mtt: Parse error: Valid type expected. Make sure uninterpreted type identifiers are capitalized and there are no unbalanced parentheses
  [124]

### This is unfortunately unrecognized (putting here to document it)
  $ mtt parse -e "fun (x : a). x"
  mtt: Parse error: Unrecognized syntax error. Please report your example
  [124]

### Missing open parenthesis
  $ mtt parse -e "fun x : A -> B). x"
  mtt: Parse error: Separator between bound variable and lambda body is missing. Use e.g. => or . Also, make sure parentheses are balanced
  [124]

### Missing closing parenthesis
  $ mtt parse -e "fun x : (A -> B. x"
  mtt: Parse error: Missing closing parenthesis at type level. Or unexpected lexeme, like => is used instead of ->. Or you are using (A, B) instead of A * B
  [124]

### The wrong arrow as a separator between the lambda parameter and its body
  $ mtt parse -e "fun x : A -> x"
  mtt: Parse error: Valid codomain type is expected. Sometimes this happens if -> is used instead of =>
  [124]

### Missing box's argument
  $ mtt parse -e "fun x : []. x"
  mtt: Parse error: Type-level box must be followed by a type
  [124]

### Use a star or a cross in product types, not a comma
  $ mtt parse -e "fun x : (A, B). x"
  mtt: Parse error: Missing closing parenthesis at type level. Or unexpected lexeme, like => is used instead of ->. Or you are using (A, B) instead of A * B
  [124]

## STLC terms

### Binary function application must be parenthesized
$ mtt parse -e "f x y"
mtt: Parse error: Binary application must be parenthesized like so: (f x) y
[1]

### Parens instead of angle brackets for pairs
  $ mtt parse -e "fun x : A. (x, x)"
  mtt: Parse error: Missing closing parenthesis or missing obligatory parentheses for multi-arg function application: (f x) y or maybe using parentheses instead of angle brackets for pairs
  [124]


## Modal terms

### Modal identifier in the wrong context
  $ mtt parse -e "fun x' : A. x"
  mtt: Parse error: A regular identifier expected. This happens, when e.g. a modal identifier is used fun x' : T. ...
  [124]

### box needs to be applied to an expression
$ mtt parse -e "box 42"
mtt: Parse error: Boxed expression is expected
[1]

### letbox is one single keyword, not two
  $ mtt parse -e "let box x' = box () in x'"
  mtt: Parse error: Sometimes this happens when you say "let box" instead of "letbox" or try to apply a function to "box"
  [124]

### letbox needs its bound var to end with apostrophy
  $ mtt parse -e "letbox x = box () in x"
  mtt: Parse error: A modal identifier is expected. It should start with a lowercase letter and end with an apostrophe (').
  [124]

## Regular "let .. in" expression

### Bound variable in "let" expression must be term, not type
  $ mtt parse -e "let x = []"
  mtt: Parse error: Variable in "let" expression must be term, not type
  [124]

### After "in" must term must follow, not type
  $ mtt parse -e "let x = () in []"
  mtt: Parse error: Expected term after "in"-keyword, not type
  [124]

### Extra closed parenthesis
  $ mtt parse -e "let x = ())"
  mtt: Parse error: Missing or unexpected lexeme in parenthesized expression
  [124]

### Missing "in" keyword
  $ mtt parse -e "let x = fun y: A. y"
  mtt: Parse error: Missing or unexpected lexeme in parenthesized expression
  [124]

  $ mtt parse -e "let x = () x"
  mtt: Parse error: Missing or unexpected lexeme in parenthesized expression
  [124]

  $ mtt parse -e "let x = () y = x"
  mtt: Parse error: Missing or unexpected lexeme in parenthesized expression
  [124]

  $ mtt parse -e "let x = (fun z: A. z) y = () in x y"
  mtt: Parse error: Missing or unexpected lexeme in parenthesized expression
  [124]

### Match-expression
  $ mtt parse <<EOF
  > match () with
  > end
  > EOF
  mtt: Parse error: Empty branch in match-expression
  [124]

  $ mtt parse <<EOF
  > match x with
  > | zero
  > end
  > EOF
  mtt: Parse error: Empty branch in match-expression
  [124]

  $ mtt parse <<EOF
  > match x
  > | zero
  > end
  > EOF
  mtt: Parse error: Missing "with" keyword
  [124]

  $ mtt parse <<EOF
  > match x with
  > | zero => ()
  > EOF
  mtt: Parse error: Empty branch in match-expression
  [124]

  $ mtt parse <<EOF
  > match x with
  > | zero => ()
  > | succ n => ()
  > EOF
  mtt: Parse error: END token wasn't found in the end of the match-expression
  [124]

  $ mtt parse <<EOF
  > match x with
  > | succ n => ()
  > | zero => ()
  > end
  > EOF
  mtt: Parse error: Incorrect match-expression
  [124]

  $ mtt parse -e "() + zero"
  mtt: Parse error: ZERO token can be used in match-expression only
  [124]
