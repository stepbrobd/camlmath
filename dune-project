(lang dune 3.21)

(generate_opam_files true)
(executables_implicit_empty_intf true)

(name camlmath)
(version 2026.903.0)
(license MIT)
(authors "Yifei Sun <ysun@hey.com>")
(maintainers "Yifei Sun <ysun@hey.com>")

(source
 (github stepbrobd/camlmath))

(package
 (name camlmath)
 (synopsis "TeX math to MathML Core converter")
 (description "Converts a subset of TeX math to MathML Core markup.")
 (tags
  (tex latex math mathml markup))
 (depends
  (ocaml
   (>= 4.14))
  (alcotest :with-test)
  (bisect_ppx :with-dev-setup)
  (odoc :with-doc)))
