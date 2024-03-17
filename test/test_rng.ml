open Rngtest

let rand4 () = Random4.float 1.
let rand5 () = Random5.float 1.

let () =
  Owl_stats_prng.init 42;
  Random.init 42

let dsfmt () = Owl.Stats.std_uniform_rvs ()

let rdrand =
  let rng = Cryptokit.Random.hardware_rng () in
  let buf = Bytes.create 4 in
  fun () ->
  rng#random_bytes buf 0 4;
  Bytes.get_int32_ne buf 0

let fortuna =
  let buf = Bytes.create 4 in
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna);
  fun () ->
  Mirage_crypto_rng.generate_into buf 4;
  Bytes.get_int32_le buf 0

let () =
  let gen = match Sys.argv.(1) with
  | "4" -> float "Random4" rand4
  | "5" -> float "Random5" rand5
  | "d" -> float "dSFMT(owl)" dsfmt
  | "f" -> int32 "Fortuna(mirage-crypto-rng)" fortuna
  | "r" -> int32 "rdrand(cryptokit)" rdrand
  | x -> invalid_arg x
  in
  Printf.printf "Testing %s\n" gen.name;
  run_all gen
