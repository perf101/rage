let[@inline always] gen () = Random5.float 1.

let () =
  Rngtest.RunTestU01.run_01 "random5" Random5.self_init gen
