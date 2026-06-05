(lang dune 3.21)

(name qmljs)

(generate_opam_files false)

(package
 (name qmljs)
 (depends
  (ocaml (>= 5.2))
  (dune (>= 3.21))
  eio
  (eio_main :with-test)))
