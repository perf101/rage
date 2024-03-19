open Rngtest

let rand4 () = Random4.float 1.
let rand5 () = Random5.float 1.

let () =
  Owl_stats_prng.init 42;
  Random.init 42

let dsfmt () = Owl.Stats.std_uniform_rvs ()

let rdrand =
  let rng = Cryptokit.Random.hardware_rng () in
  let buf = Bytes.create 0x10000 in
  let off = ref (Bytes.length buf) in
  fun () ->
  if !off + 4 > Bytes.length buf then begin
    rng#random_bytes buf 0 (Bytes.length buf);
    off := 0
  end;
  let r = Bytes.get_int32_ne buf !off in
  off := !off + 4;
  r

let fortuna =
  let random = ref "" in
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna);
  let off = ref 0 in
  fun () ->
  if !off + 4 > String.length !random then begin
    random := Mirage_crypto_rng.generate 0x10000;
    off := 0;
  end;
  let r = String.get_int32_ne !random !off in
  off := !off + 4;
  r

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
  if Array.length Sys.argv > 2 then
    run_custom gen (int_of_string Sys.argv.(2))
  else
    run_all gen
