let[@inline always] gen () = Random5.float 1.

let () =
  Rngtest.stream_floats Random5.self_init gen
