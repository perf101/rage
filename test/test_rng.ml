open TestU01

(* TestU01 doesn't accept 1 as a random value, need to be [0, 1),
   but both Owl and OCaml's RNG has inclusive bounds
 *)

let m1 = Float.pred 1.

let float01 () = Random.float m1

let owl () = Owl_stats_dist.uniform_rvs ~a:0. ~b:m1

let () =
  Owl_stats_prng.self_init ();
(*  let gen = Unif01.create_extern_gen_01 "owl" owl in*)
  let gen = Unif01.create_extern_gen_01 "stdlib" float01 in
  let rep = Array.make (1 + Bbattery.ntests_crush) 0 in
  rep.(32) <- 10;
(*  Bbattery.small_crush gen; *)
  Bbattery.repeat_crush gen rep
