# Dependencies useful for development and CI
# Check the required ocamlformat's version in .ocamlformat file
DEV_DEPS := \
merlin \
ocamlformat \
ocp-indent \
utop

all:
	dune build

test:
	dune runtest

# Autoformat sources inplace (needs ocamlformat installed)
fmt:
	dune build @fmt --auto-promote

# CI: lint OCaml and dune source files, all the opam files in the project root
lint:
	dune build @fmt
	opam lint .

dev-deps:
	opam install -y $(DEV_DEPS)

clean:
	dune clean || true

.PHONY: all test fmt lint dev-deps clean
