let init () =
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

let gen () buf n =
  let rec loop buf_off =
    let f =  Mirage_crypto_rng.Entropy.cpu_rng_bootstrap |> Result.get_ok in
    let s = try f 0 with Failure _ -> "12" in
    let off = 2 in
    let len = Int.min (n - buf_off) (String.length s - off) in
    if len > 0 then begin
      Bytes.blit_string s off buf buf_off len;
      loop (buf_off + len)
    end
      (* TODO: this segfaults !! 
    Printf.eprintf "%S\n%!" (Bytes.to_string buf) *)
  in
  loop 0

let () =
  Rngtest.RunTestU01.run_buf "mirage-crypto-fortuna" init gen
