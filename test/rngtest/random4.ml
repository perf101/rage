let[@inline always] gen () = Random4.float 1.
let () =
  Rngtest.RunTestU01.run_01 __MODULE__ Random4.self_init gen
