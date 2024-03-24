let uint32_max = (2. ** 32.) -. 1.

(** [int32_of_01 f] returns a 32-bit unsigned integer coresponding to the [f] value.

  @param f floating-point value in the range [[0, 1]]
*)
let[@inline always] int32_of_01 f =
  (* cannot convert directly with [Int32.of_float], because it is documented
     to return an unspecified integer when the argument is out of range
     (and in particular returns the same constant value for all values that are out of range).
     Whereas the [Int64.to_int32] is documented to work modulo [2^{32}]
  *)
  f *. uint32_max |> Int64.of_float |> Int64.to_int32

(** [f01_of_int32 i32] returns a floating-point value in the range [[0, 1]],
  interpreting [i32] as an unsigned 32-bit integer.

  @param i32 an unsigned 32-bit integer
 *)
let[@inline always] f01_of_int32 i32 =
  let f = Int64.(logand (of_int32 i32) 0xFFFF_FFFFL |> to_float) in
  f /. uint32_max

let () =
  assert (int32_of_01 0. = 0l) ;
  assert (int32_of_01 0.5 = Int32.max_int) ;
  assert (int32_of_01 1. = -1l);
  assert (f01_of_int32 0l = 0.);
  assert (abs_float (f01_of_int32 Int32.max_int -. 0.5) < 1e-6);
  assert (f01_of_int32 (-1l) = 1.)

(** [float_of_buf gen] converts a buffered generatoor to a floating-point value generator.

  @param gen generates random bits into a supplied buffer
 *)
let[@inline always] float_of_buf gen =
  let buf = Bytes.create 0x100_000 in
  let off = ref (Bytes.length buf) in
  let[@inline never] refill () =
      gen buf 0 (Bytes.length buf);
      off := 0
  in
  fun () ->
    if !off + 4 >= Bytes.length buf then refill ();
    let f = Bytes.get_int32_ne buf !off |> f01_of_int32 in
    off := !off + 4;
    f

(** [nopipefail ()] exits with code [0] when [SIGPIPE] is received.
  This is needed because [dune] would run this program with [-o pipefail],
  and when testing a RNG we'd supply an endless stream of data and expect to get SIGPIPE when the test finishes
  on the other side.
 *)
let nopipefail () =
 let (_:Sys.signal_behavior) = Sys.signal Sys.sigpipe (Sys.Signal_handle (fun _ -> exit 0)) in
 ()

(** [stream_buf seed gen] writes a continous stream of bits on standard output.

  @param seed initializes the RNG
  @param gen generates random bits in the supplied buffer
 *)
let[@inline always] stream_buf seed gen =
  let rng =  seed () in
  nopipefail ();
  let n = 0x10_000 in
  let buf = Bytes.create n in
  while true do
    gen rng buf n ;
    if Unix.write Unix.stdout buf 0 n <> n then
      exit 1
  done

(** [stream_floats seed gen] writes a continous stream of bits on standard output.

  @param seed initializes the RNG
  @params gen generates floats in the range [[0., 1.]]
 *)
let stream_floats seed gen =
  stream_buf seed @@ fun () buf n ->
  for i = 0 to (n - 1) / 4 do
    gen () |> int32_of_01 |> Bytes.set_int32_ne buf (4 * i)
  done

(** [gen_01 name seed gen] is a {!module:TestU01.Unif01.gen} RNG.

  @param name the name of the generator
  @param seed the function to initialize the generator with a seed
  @param gen a generator that returns floats in the [[0, 1]] range.
 *)
let gen_01 name seed gen =
  seed () ;
  let wrap () =
    let f = gen () in
    if f = 1. then
     (* unlikely, but it did happen a few times, and TestU01 rejects and exact 1.
        Don't loop to generate a new one, so that we can detect stuck-at-1 errors.
     *)
     Float.pred f
    else f
  in
  TestU01.Unif01.create_extern_gen_01 name wrap

(** [gen_buf name seed gen] is a {@module:TestU01.Unif01.gen} RNG.

  @param name the name of the generator
  @param seed the function to initialize the generator with a seed
  @param gen a generates random bits into a supplied buffer
 *)
let gen_buf name seed gen =
  gen_01 name seed (float_of_buf gen)
