(lang dune 3.24)

(generate_opam_files true)

(package
 (name sysploc)
 (depends
  (ocaml
   (>= 5.1))
  (dune
   (>= 3.24))
  (yojson
   (>= 3.0))))
