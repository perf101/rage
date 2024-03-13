(lang dune 2.0)
(generate_opam_files)
(name rage)
(source (github perf101/rage))
(license BSD3)
(package
 (name rage)
 (synopsis "RAGE, Results And Graphing Engine")
 (depends
  (core (and (>= v0.13) (< v0.15)))
  (async (and (>= v0.13) (< v0.15)))
  (postgresql (>= 4.5.2))
  (ocurl (and (>= 0.9.0) (< 1.0)))
  ezjsonm
  owl
  (ssl (>= 0.5.7))
  ppx_sexp_conv
  re
  uri))
