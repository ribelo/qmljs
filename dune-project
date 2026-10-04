(lang dune 3.21)

(name qmljs)

(license Unlicense)

(generate_opam_files false)

(package
 (name qmljs)
 (depends
  (ocaml (= 5.4.0))
  (ocaml-variants (= 5.4.0+ox))
  (dune (>= 3.21))
  eio
  (eio_main :with-test)
  (qcheck :with-test)))

(package
 (name qmljs_ppx)
 (allow_empty)
 (depends
  (ocaml (= 5.4.0))
  (ocaml-variants (= 5.4.0+ox))
  (dune (>= 3.21))
  qmljs
  ppxlib))
