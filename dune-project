(lang dune 3.21)

(name quick_ml)

(generate_opam_files false)

(package
 (name quick_ml)
 (depends
  (ocaml (>= 5.2))
  (dune (>= 3.21))
  eio
  (eio_main :with-test)))
