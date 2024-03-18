open Rngtest
open Alcotest.V1
open Datagen

let gen_uniform = f01 "Uniform.rng" Uniform.rng

let () =
  Random.init 42;
  run "Datagen"
  [ "RNG",
   [ test_case "TestU01 SmallCrush" `Quick (fun () -> run_testu01 small_crush gen_uniform)
   ; test_case "TestU01 custom" `Quick (fun () -> Random.init 42; run_custom_small gen_uniform 1)
   ]
    
  ]
