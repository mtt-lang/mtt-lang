# Modal Type Theory implementation

![GitHub Actions][github-actions-shield]

[github-actions-shield]: https://github.com/anton-trunov/modal-type-theory/workflows/Build/badge.svg

This is a *work-in-progress* simple implementation of (a part of) the modal type
theory by Davies and Pfenning described in their [A Modal Analysis of Staged
Computation(2001)][DP2001] paper. For a, perhaps, gentler introduction, see
Pfenning's lecture notes accompanying his and Platzer's [course on modal
logic][course].

You might want to check out the [project's wiki][wiki] for a selection of
related papers and projects.

**Disclaimer**: this implementation has not been extensively tested yet, so it
might contain critical bugs.

[DP2001]: https://www.cs.cmu.edu/~fp/papers/jacm00.pdf
[course]: https://www.cs.cmu.edu/~fp/courses/15816-s10
[wiki]: https://github.com/mtt-lang/modal-type-theory/wiki


## Contributing

Your contribution is very welcome. Please check the details in
[CONTRIBUTING.md](./CONTRIBUTING.md). The [HACKING.md](./HACKING.md) file
explains how to build and test the project, format its code and things like
that.

## How to use

You can try out the typechecker and evaluator online: https://mtt-lang.github.io/mtt-web or
install `mtt` locally using the [build instructions](./HACKING.md).

Here is how you can use a local `mtt` setup. Use `mtt --help` to list all
the subcommands `mtt` supports. Each of those subcommands also supports `--help`
flag. Here are some examples:

- Typechecking a term:

  ```
  $ mtt check "[]A -> A" -e "λx : []A. letbox u' = x in u'" --verbose
  OK. Expression typechecks.
  ```

- Inferring the type of a term:

  ```
  $ mtt infer -e "λx : []A. letbox u' = x in u'"
  (□A → A)
  ```
  Yes, Unicode is allowed (The `□` symbol is the *box* type constructor).

  Here is an example which must not typecheck because a box tries to capture a
  regular variable:

  ```
  $ mtt infer -e "λf:B -> []A. λy:B. (λx:[]A. box x) (f y)"
  mtt: Type inference error: Variable x is not found in the regular context!
  ```

- Inferring the type of a term from `stdin` using [heredoc](https://en.wikipedia.org/wiki/Here_document) syntax:
  ```
  mtt infer << EOF
  fun x:[]A. fun y:[]B.
    letbox x' = x in
    letbox y' = y in
    box <x', y'>
  EOF
  (□A → (□B → □(A×B)))
  ```

- Evaluating a term from a file ([examples/eval-apply.mtt](./examples/eval-apply.mtt)):

  ```
  $ mtt eval examples/eval-apply.mtt
  ()
  ```
  
- Parsing and pretty-printing (which is not really pretty at the moment) are
  also exposed for the purposes of testing the implementation:

  ```
  $ mtt parse examples/eval-apply.mtt
  (λx : □(). letbox u' = x in u')
  (((λx : □(() → ()). λy : □(). letbox u' = x in
  letbox w' = y in box (u' w'))
  (box (λx : (). x)))
  (box ((λx : (). x) ())))
  ```

## Language Syntax

### Numerals

Unsigned decimal numerals are supported.

### Identifiers

- The regular lambda calculus identifiers (*Lid*) called *regular* start with a
  lowercase letter followed by any number of alphanumeric characters or
  underscores (`_`).
- The valid, or *modal*, identifiers (*Gid*) are syntactically the same as the
  regular ones except that they *must* end with an apostrophe (`'`).
- *Uninterpreted type* identifiers (*Tid*) start with a capital letter followed
  by any number of alphanumeric characters or underscores (`_`).

### Keywords

Here are the keywords `fun`, `in`, `let`, `box`, `letbox`, `fst`, `snd`,
`match`, `with`, `end`, `zero`, `succ`. The keywords *cannot* be used as
identifiers.

### Other lexemes

Pairs are denoted with angle brackets `>`, `<` separated with a comma (`,`).
Parentheses (`)` and `(`) are used as usual for syntactical disambiguation both
at the type and term levels and to optionally parenthesize bound regular
variables and their type annotations. The unit type and its only value are both
denoted with `()`. The dot (`.`) or the double arrow (`=>`) is used to separate
bound variables from abstractions' bodies. The equals sign (`=`) is used as a
separator in `letbox`- expressions. The pipe symbol (`|`) is used to separate
the branches of the pattern-matching expression.

### Unicode 

The following table specifies the correspondence between the ASCII lexemes and
the Unicode ones.

| ASCII       | Unicode   | Meaning                        |
| :---------: | :-------: | :----------------------------: |
| `[]`        | `□`       | Box modality                   |
| `*`         | `×`       | Product type or multiplication |
| `->`        | `→`       | Arrow type                     |
| `fun`       | `λ`       | Lambda abstraction             |
| `fst`       | `π₁`      | First projection               |
| `snd`       | `π₂`      | Second projection              |
| `.`/ `=>`   | `⇒`       | Separator                      |
| `Nat`       | `ℕ`       | Natural numbers type           |

### Abstract syntax

#### Types

| *T* ::=   |               | Meaning                      |
| :-------: | :-----------: | :--------------------------: |
|           | `()`          | Unit type                    |
|           | `ℕ`           | Natural numbers type         |
|           | *Tid*         | Uninterpreted types          |
|           | `□` *T*       | Type of staged expressions   |
|           | *T* `×` *T*   | Type of pairs                |
|           | *T* `→` *T*   | Type of functions            |

#### Terms (expressions)

| *t* ::=   |                                                                  | Meaning                                                   |
| :-------: | :--------------------------------------------------------------: | :-------------------------------------------------------: |
|           | `()`                                                             | the only inhabitant of the unit type                      |
|           | *numerals*                                                       | Natural numbers                                           |
|           | *Lid*                                                            | Regular variable                                          |
|           | *Gid*                                                            | Modal (valid) variable                                    |
|           | `<` *t* `,` *t* `>`                                              | Pair expression                                           |
|           | `π₁` *t*                                                         | First projection from a pair                              |
|           | `π₂` *t*                                                         | Second projection from a pair                             |
|           | `λ` *Lid* `:` *T* `.` *t*                                        | Lambda abstraction with explicitly typed variable         |
|           | `λ` `(` *Lid* `:` *T* `)` `.` *t*                                | Lambda abstraction with explicitly typed modal variable   |
|           | *t* *t*                                                          | Function application                                      |
|           | `box` *t*                                                        | Staged computations                                       |
|           | `let` *Lid* `=` *t* `in` *t*                                     | Let-expression                                            |
|           | `letbox` *Gid* `=` *t* `in` *t*                                  | Running staged computations                               |
|           | *t* `+` *t*                                                      | Addition                                                  |
|           | *t* `-` *t*                                                      | Truncation substraction (`0 - n` evaluates to `0`)        |
|           | *t* `*` *t*                                                      | Multiplication                                            |
|           | *t* `/` *t*                                                      | Division (`n / 0` throws run-time error)                  |
|           | `match` *t* `with \| zero =>` *t* `\| succ` *Lid* `=>` *t* `end` | Pattern-matching on natural numbers                       |

#### Values

| *v* ::=   |                                   | Meaning                                |
| :-------: | :-------------------------------: | :------------------------------------: |
|           | `()`                              | the only inhabitant of the unit type   |
|           | *numerals*                        | Natural numbers                        |
|           | `<` *v* `,` *v* `>`               | Pair value                             |
|           | `λ` *Lid* `.` *t*                 | Lambda abstraction value               |
|           | `box` *t*                         | Staged computation                     |
