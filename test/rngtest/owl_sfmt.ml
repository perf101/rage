let () =
  Rngtest.RunTestU01.run_01 "owl-SFMT" Owl_stats_prng.self_init Owl.Stats.std_uniform_rvs
