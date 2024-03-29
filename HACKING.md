# How to hack on this project

## How to build

An easy way to build the project is using the [opam](https://opam.ocaml.org)
package manager for OCaml.

### Installing dependencies into local opam switch

I'll show how to install the project dependencies. By the way, the dependencies
are recorded in the generated [mtt.opam](./mtt.opam) file).

Once you have opam installed, go to the project root directory and execute the
following command which will create a *local* switch with the specified version
of OCaml compiler, then opam will download, compile and install the
dependencies. By default, local switch will be available *only* inside this
project.

```shell
opam switch create ./ --deps-only --with-test ocaml-base-compiler.4.08.1
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
  compile and run the modified CLI-driver `mtt`:

  ``` shell
  $ dune exec -- mtt infer examples/apply.mtt
  ```

- Another option is to install `mtt` into your switch `opam install ./mtt.opam`
  and use it as shown at the beginning of this README file:

  ``` shell
  $ mtt eval examples/eval-apply.mtt
  ```

## Running/promoting tests

- To run tests execute `make test` or `dune runtest`.
- To accept changes to the existing tests, e.g. due to a new format of output or
  new CLI exit codes, run `dune promote` or `make gold`.

## Updating the language grammar

The project uses [Menhir][menhir]'s incremental API to make error reporting more
meaningful without resorting to writing a parser by hand.

The file [ParserErrors.messages](./src/ParserErrors.messages) contains the
states of the parser automaton and the corresponding error messages. The states
are generated by Menhir and the messages are written by hand. The parser error
tests are located in [parser-errors.t](test/parser/parser-errors.t) file.

Every time you update the language grammar you regenerate the
`ParserErrors.messages` file using `make update-messages` command and add tests
to `parser-errors.t`.

[menhir]: http://gitlab.inria.fr/fpottier/menhir

## Code formatting

To format source code, install the [ocamlformat][ocamlformat] tool and use `make
fmt` command to autoformat your changes. Make sure you use the version of ocamlformat
as specified in the [.ocamlformat](./.ocamlformat) file.

Our CI uses linters for OCaml and Dune files and will flag an error if the
source code is not properly formatted.

[ocamlformat]: https://github.com/ocaml-ppx/ocamlformat
