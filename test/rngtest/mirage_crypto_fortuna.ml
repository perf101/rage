let init () =
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

let gen () buf n =
  Mirage_crypto_rng.generate_into buf n

let () =
  Rngtest.RunTestU01.run_buf "mirage-crypto-fortuna" init gen
