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

promote:
	dune promote

# an alias for promoting tests
gold:
	dune promote

# Autoformat sources inplace (needs ocamlformat installed)
fmt:
	dune build @fmt --auto-promote

# Update src/ParserErrors.messages
update-messages:
	menhir --list-errors src/Parser.mly >src/NewParserErrorsStubs.messages
	menhir --merge-errors src/ParserErrors.messages \
		   --merge-errors src/NewParserErrorsStubs.messages \
		   src/Parser.mly  >src/NewParserErrors.messages
	mv src/NewParserErrors.messages src/ParserErrors.messages
	rm src/NewParserErrorsStubs.messages

# CI: lint OCaml and dune source files, all the opam files in the project root
lint:
	dune build @fmt
	opam lint .

dev-deps:
	opam install -y $(DEV_DEPS)

clean:
	dune clean || true

.PHONY: all test gold fmt lint dev-deps clean
