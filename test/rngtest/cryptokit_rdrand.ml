let[@inline always] gen rng buf n =
  rng#random_bytes buf 0 n

let () =
  Rngtest.RunTestU01.run_buf "cryptokit-rdrand" Cryptokit.Random.hardware_rng gen
