(lang dune 3.14)
(using mdx 0.4)
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
  (owl (>= 1.1))
  (owl-base (>= 1.1))
  (ssl (>= 0.5.7))
  ppx_sexp_conv
  ppx_let
  re
  (cryptokit (and :with-dev-setup (>= 1.19)))
  (mirage-crypto-rng (and :with-dev-setup (>= 0.11.3)))
  (stdlib-random (>= 1.1.0))
  (testu01 (and :with-test (>= 1.2.3-0.2)))
  (mdx :with-test)
  cpu
  uri))
