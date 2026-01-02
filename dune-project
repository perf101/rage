(lang dune 2.0)
(generate_opam_files)
(name rage)
(source (github perf101/rage))
(license BSD3)
(package
 (name rage)
 (synopsis "RAGE, Results And Graphing Engine")
 (depends
  (core (>= v0.13))
  (async (>= v0.13))
  (postgresql (>= 4.5.2))
  (ocurl (>= 0.9.0))
  owl
  (ssl (= 0.5.7))
  ppx_sexp_conv
  re
  uri))
