(lang dune 2.7)
(cram enable)
(using menhir 2.0)
(name mtt)

(implicit_transitive_deps true)

(license MIT)
(maintainers "Anton Trunov")
(authors "Anton Trunov")
(source (github anton-trunov/mtt))

(generate_opam_files true)

(package
 (name mtt)
 (synopsis "Modal Type Theory")
 (description "
Based on Modal Type Theory as given by F. Pfenning et al.")
 (depends
  (base (and (>= v0.12.2) (< v0.15.0)))
  cmdliner
  (dune (>= 2.0))
  js_of_ocaml
  js_of_ocaml-compiler
  js_of_ocaml-lwt
  js_of_ocaml-tyxml
  js_of_ocaml-ppx
  (menhir (>= 20201201))
  (ocaml (>= 4.07.0))
  (ocamlformat (and :with-test (= 0.16.0)))
  (pprint (>= 20220103))
  ppx_compare
  ppx_let
  ppx_sexp_conv
  ppx_string_interpolation
  qcheck
  sedlex
  sexplib
  stdio
  zarith
  zarith_stubs_js
  ))
