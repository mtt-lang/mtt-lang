# Contributing to MTT

## License

By contributing to this repository, you agree that your contributions will be
licensed under the MIT license, which is copied into the [LICENSE][license] file
in the root of the project.

[license]: ./LICENSE

## Style guide

- Commit messages style: https://www.conventionalcommits.org/en/v1.0.0/#summary. See also https://commitlint.io.
- Document the changes: https://keepachangelog.com/en/1.0.0/.
- Format the OCaml source code using the [ocamlformat][ocamlformat] tool.
- CI uses linters for OCaml and Dune files, use `make fmt` command to autoformat
  your changes, otherwise the changes might not pass CI.

[ocamlformat]: https://github.com/ocaml-ppx/ocamlformat
