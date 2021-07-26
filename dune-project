(lang dune 2.8)

(name um-mchk)

(generate_opam_files true)

(source (github shonfeder/um-mchk))
(license MIT)
(authors "Shon Feder")
(maintainers "shon.feder@gmail.com")
(documentation "https://shonfeder.github.io/um-mchk")

(package
 (name um-mchk)
 (synopsis
  "TODO")
 (description
  "TODO")
 (depends
  (bos (>= 0.2.0))
  (ppx_compare (>= v0.14.1))
  (ppx_sexp_conv (>= v0.14.1))
  (ppx_expect (and :with-test (>= v0.14.1)))
  (qcheck-core (and :with-test (>= 0.17)))))
