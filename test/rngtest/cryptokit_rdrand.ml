let[@inline always] gen rng buf n =
  rng#random_bytes buf 0 n

let () =
  Rngtest.stream_buf Cryptokit.Random.hardware_rng gen
