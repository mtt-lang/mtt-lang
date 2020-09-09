# Modal Type Theory implementation

![GitHub Actions][github-actions-shield]

[github-actions-shield]: https://github.com/anton-trunov/modal-type-theory/workflows/Main%20workflow/badge.svg

This is a *work-in-progress* simple implementation of (a part of) the modal type
theory by Davies and Pfenning described in their [A Modal Analysis of Staged
Computation(2001)][DP2001] paper. For a, perhaps, gentler introduction, see
Pfenning's lecture notes accopmanying his and Platzer's [course on modal
logic][course].

**Disclaimer 1**: this implementation has not been extensively tested yet, so it
might contain critial bugs.

**Disclaimer 2**: error reporting mechanism does not report locations yet. This
is easy to add for the parsing phase but requires more work to do e.g. during
typechecking as I don't keep location information yet. Among other glaring
omissions is the lack of support for source code comments, regular
`let`-expressions and type ascriptions for terms.

[DP2001]: https://www.cs.cmu.edu/~fp/papers/jacm00.pdf
[course]: https://www.cs.cmu.edu/~fp/courses/15816-s10


## Contributing

Your contribution is very welcome. Please check the details in [CONTRIBUTING.md](./CONTRIBUTING.md).

## How to use

- Typechecking a term:

  ```
  $ mtt check -e "λx : []A. letbox u' = x in u'" -t "[]A -> A"
  OK. Term typechecks! 
  ```

- Inferring the type of a term:

  ```
  $ mtt infer -e "λx : []A. letbox u' = x in u'"
  (□A → A)
  ```
  Yes, Unicode is allowed.

  Here is an example which must not typecheck because a box tries to capture a
  local variable:

  ```
  $ mtt infer -e "λf:B -> []A. λy:B. (λx:[]A. box x) (f y)"
  Type inference error: Variable x is not found in the local context!
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
  ((λx:□(). (letbox u' = x in u'))
  (((λx:□(() → ()). (λy:□(). (letbox u' = x in
  (letbox w' = y in (box (u' w'))))))
  (box (λx:(). x)))
  (box ((λx:(). x) ()))))
  ```

## How to build

An easy way to build the project is using the [opam](https://opam.ocaml.org)
package manager for OCaml.

### Installing dependencies into local opam switch

I'll show how to install the project dependencies. By the way, the dependencies
are recorded in the generated [mtt.opam](./mtt.opam) file).

Once you have opam installed, go to the project root directory and execute the
following command which will create a *local* switch with the specified version
of OCaml compiler, then it opam will download, compile and install the
dependencies. Local switch will be by default available *only* inside this
project.

```shell
opam switch create ./ --deps-only --with-test ocaml-base-compiler.4.07.1
```

Note that this will create `_opam` directory at the project root with all the
compiled libraries and tools used in this project, so be careful with commands
like `git clean` which may remove `_opam` (e.g. use something like this `git
clean -dfX --exclude=\!_opam/**`).

### Installing dependencies into existing opam switch

If you have an existing opam switch you'd like to reuse, simply run the
following command

```shell
opam install ./mtt.opam --with-test --deps-only
```


## How to run `mtt` command

- My workflow is as follows: I modify the source code and play with the
  evaluator or typechecker using [dune](https://dune.build) build system to
  compile and run the modified CLI-driver `mtt` (note that dune needs the `exe`
  extension):

  ``` shell
  $ dune exec bin/mtt.exe -- infer examples/apply.mtt
  ```

- Another option is to install `mtt` into your switch `opam install ./mtt.opam`
  and use it as shown at the beginning of this README file:

  ``` shell
  $ mtt eval examples/eval-apply.mtt
  ```


## Language Syntax

### Identifiers

- The regular lambda calculus identifiers (*Lid*) called *local* start with a
  lowercase letter followed by any number of alphanumeric characters or
  underscores (`_`).
- The valid, or *global*, identifiers (*Gid*) are syntactically the same as the
  local ones except that they *must* end with an apostrophe (`'`).
- *Uninterpreted type* identifiers (*Tid*) start with a capital letter followed
  by any number of alphanumeric characters or underscores (`_`).

### Keywords

Here are the keywords `fun`, `in`, `box`, `letbox`, `fst`, `snd`. The keywords
*cannot* be used as identifiers.

### Other lexemes

Pairs are denoted with angle brackets `<`, `>` separated with a comma (`,`).
Parentheses (`)` and `(`) are used as usual for syntactical disambiguation both
at the type and term levels and to optionally parenthesize bound local variables
and their type annotations. The unit type and its only value are both denoted
with `()`. The dot (`.`) or the double arrow (`=>`) is used to separate bound
variables from abstractions' bodies. The equals sign (`=`) is used as a
separator in `letbox`- expressions.

### Unicode 

The following table specifies the correspondence between the ASCII lexemes and
the Unicode ones.

| ASCII     | Unicode | Meaning            |
|:---------:|:-------:|:------------------:|
| `[]`      | `□`     | Box modality       |
| `*`       | `×`     | Product type       |
| `->`      | `→`     | Arrow type         |
| `fun`     | `λ`     | Lambda abstraction |
| `fst`     | `π₁`    | First projection   |
| `snd`     | `π₂`    | Second projection  |
| `.`/ `=>` | `⇒`     | Separator          |

### Abstract syntax

#### Types

| *T* ::= |             | Meaning                    |
|:-------:|:-----------:|:--------------------------:|
|         | `()`        | Unit type                  |
|         | *Tid*       | Uninterpreted types        |
|         | `□` *T*     | Type of staged expressions |
|         | *T* `×` *T* | Type of pairs              |
|         | *T* `→` *T* | Type of functions          |


#### Terms (expressions)

| *t* ::= |                                   | Meaning                                                 |
|:-------:|:---------------------------------:|:-------------------------------------------------------:|
|         | `()`                              | the only inhabitant of the unit type                    |
|         | *Lid*                             | Local (regular) variable                                |
|         | *Gid*                             | Global (valid) variable                                 |
|         | `<` *t* `,` *t* `>`               | Pair expression                                         |
|         | `π₁` *t*                          | First projection from a pair                            |
|         | `π₂` *t*                          | Second projection from a pair                           |
|         | `λ` *Lid* `:` *T* `.` *t*         | Lambda abstraction with explicitly typed local variable |
|         | `λ` `(` *Lid* `:` *T* `)` `.` *t* | Lambda abstraction with explicitly typed local variable |
|         | *t* *t*                           | Function application                                    |
|         | `box` *t*                         | Staged computations                                     |
|         | `letbox` *Gid* `=` *t* `in` *t*   | Running staged computations                             |

#### Values (literals)

| *v* ::= |                                 | Meaning                              |
|:-------:|:-------------------------------:|:------------------------------------:|
|         | `()`                            | the only inhabitant of the unit type |
|         | `<` *v* `,` *v* `>`             | Pair value                           |
|         | `λ` *Lid* `.` *t*               | Lambda abstraction value             |
|         | `box` *t*                       | Staged computation                   |
