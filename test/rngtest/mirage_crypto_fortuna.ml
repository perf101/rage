let init () =
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

let gen () buf n =
  Mirage_crypto_rng.generate_into buf n

let () =
  Rngtest.stream_buf init gen
