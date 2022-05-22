# REPL

Enter `dune exec -- repl` to start MTT REPL. To interact with the terminal the [lambda-term](https://github.com/ocaml-community/lambda-term) library is used.

## Available commands:

| Command          |            Meaning                 |
| :--------------: | :--------------------------------: |
| `<term>`         | evaluate/run `<term>`              |
| `:eval <term>`   | evaluate/run `<term>`              |
| `:infer <term>`  | infer the type of a `<term>`       |
| `:print <term>`  | parse and pretty-print a `<term>`  |
| `:help`          | print help message                 |
| `:exit`          | exit REPL                          |

