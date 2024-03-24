let[@inline always] gen () = Random4.float 1.
let () =
  Rngtest.stream_floats Random4.self_init gen
