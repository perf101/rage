module WW = struct
  type g = unit

  let block = 200

  let create ?time:_ () = ()

  let rec generate_into ~g:() buf ~off n =
    if n > 0 then begin
      let s = Mirage_crypto_rng.Entropy.whirlwind_bootstrap 0 in
      let src_off = 2 in (* drop header *)
      let len = Int.min (String.length s - src_off) n in
      Bytes.blit_string s src_off buf off len;
      generate_into ~g:() buf ~off:(off + len) (n - len)
      (* TODO: this segfaults !! 
    Printf.eprintf "%S\n%!" (Bytes.to_string buf) *)
    end
    
  let reseed ~g:() _ = ()
  let accumulate ~g:() _ = `Acc (fun _data -> ())

  let seeded ~g:() = true
  let pools = 0
end

let init () =
  Mirage_crypto_rng_unix.initialize (module WW)

let gen () buf n = Mirage_crypto_rng.generate_into buf n

let () =
  Rngtest.RunTestU01.run_buf "mirage-crypto-fortuna" init gen
