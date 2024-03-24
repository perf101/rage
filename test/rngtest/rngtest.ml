(* 
  See http://simul.iro.umontreal.ca/testu01/copyright.html, which says TestU01 got relicensed and is under Apache2.
  There is a version of that here with the changes applied to the source code: https://github.com/umontreal-simul/TestU01-2009/

*)

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
  assert (int32_of_01 1. = -1l) ;
  assert (f01_of_int32 0l = 0.) ;
  assert (abs_float (f01_of_int32 Int32.max_int -. 0.5) < 1e-6) ;
  assert (f01_of_int32 (-1l) = 1.)

(** [float_of_buf gen] converts a buffered generatoor to a floating-point value generator.

  @param gen generates random bits into a supplied buffer
 *)
let[@inline always] float_of_buf seed gen =
  let buf = Bytes.create 0x100_000 in
  let off = ref (Bytes.length buf) in
  let t = seed () in
  let[@inline never] refill () =
    gen t buf (Bytes.length buf) ;
    off := 0
  in
  fun () ->
    if !off + 4 >= Bytes.length buf then refill () ;
    let f = Bytes.get_int32_ne buf !off |> f01_of_int32 in
    off := !off + 4 ;
    f

(** [nopipefail ()] exits with code [0] when [SIGPIPE] is received.
  This is needed because [dune] would run this program with [-o pipefail],
  and when testing a RNG we'd supply an endless stream of data and expect to get SIGPIPE when the test finishes
  on the other side.
 *)
let nopipefail () =
  let (_ : Sys.signal_behavior) =
    Sys.signal Sys.sigpipe (Sys.Signal_handle (fun _ -> exit 0))
  in
  ()

(** [stream_buf seed gen] writes a continous stream of bits on standard output.

  @param seed initializes the RNG
  @param gen generates random bits in the supplied buffer
 *)
let[@inline always] stream_buf seed gen =
  let rng = seed () in
  nopipefail () ;
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

module RunTestU01 = struct
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
      else
        f
    in
    TestU01.Unif01.create_extern_gen_01 name wrap

  (** [gen_buf name seed gen] is a {@module:TestU01.Unif01.gen} RNG.

  @param name the name of the generator
  @param seed the function to initialize the generator with a seed
  @param gen a generates random bits into a supplied buffer
 *)
  let gen_buf name seed gen = gen_01 name ignore (float_of_buf seed gen)

  let index = ref (-1)

  let nbits_log2 = ref 25

  let r = ref 0

  let s = ref 32

  let w = ref (-1)

  let test = ref None

  let repeat = ref 1

  let tests =
    let get_rep n =
      if n = 0 then invalid_arg "--index cannot be 0";
      let rep = Array.make (n + 1) 0 in
      match !index with
      | -1 ->
          prerr_endline "--index not set" ;
          exit 2
      | idx ->
          rep.(idx) <- 1 ;
          rep
    in
    let get_nb () = 2. ** float_of_int !nbits_log2 in
    let make1 name f = (name, ([""], f)) in
    let gen_index n =
      List.init n (fun i -> Printf.sprintf "--index %d" (i + 1))
    in
    let maken name repeat ntests =
      (name, (gen_index ntests, fun gen -> repeat gen @@ get_rep ntests))
    in
    let makenb name repeat ntests =
      ( name
      , (gen_index ntests, fun gen -> repeat gen (get_nb ()) @@ get_rep ntests)
      )
    in
    let makenba name repeat ntests =
      ( name
      , ( gen_index ntests
        , fun gen -> repeat gen (get_nb ()) !r !s @@ get_rep ntests
        )
      )
    in
    let makenbba name repeat ntests =
      let valid_w = [1; 2; 4; 8; 16; 32] in
      let args =
        gen_index ntests
        |> List.concat_map @@ fun idx ->
           valid_w |> List.map @@ fun w -> Printf.sprintf "%s --w %d" idx w
      in
      (name, (args, fun gen -> repeat gen (get_nb ()) !r !s (get_rep ntests) !w))
    in
    let open TestU01.Bbattery in
    [
      maken "SmallCrush" repeat_small_crush ntests_small_crush
    ; make1 "PseudoDIEHARD" pseudo_diehard
    ; make1 "FIPS-140-2" fips_140_2
    ; makenb "Rabbit" repeat_rabbit ntests_rabbit
    ; makenba "Alphabit" repeat_alphabit ntests_alphabit
    ; makenbba "BlockAlphabit" repeat_block_alphabit ntests_block_alphabit
    ; maken "Crush" repeat_crush ntests_crush
    ; maken "BigCrush" repeat_big_crush ntests_big_crush
    ] |> List.rev

  let rec rerun f gen =
    let open TestU01 in
    f gen;
    let threshold = Probdist.Gofw.get_suspectp () in
    Array.combine (Bbattery.get_test_names ()) (Bbattery.get_p_val ())
    |> Array.iter @@ fun (test_name, p_value) ->
    if p_value < threshold || p_value > (1. -. threshold) then
      (* similar to the [-X] failure threshold in [dieharder]. *)
      if p_value <= 1e-6 || p_value >= (1. -. 1e-6) then begin
        Printf.eprintf "%s: p-value %g\n%!" test_name p_value;
        exit 1
      end
      else
        (* may go away on a rerun *)
       rerun f gen

  (** [run_cli gen default] runs [TestU01] tests on [gen] based on command-line arguments.
    It invokes [default] instead of no CLI arguments are present.
   *)
  let run_cli gen default =
    let args =
      Arg.align
        [
          ( "--test"
          , Arg.Symbol
              ( List.map fst tests
              , fun sym -> test := Some (List.assoc sym tests)
              )
          , "Test battery to run"
          )
        ; ("--index", Arg.Set_int index, "The test index to run")
        ; ( "--nbits_log2"
          , Arg.Set_int nbits_log2
          , "Number of bits as a power of 2 (Rabbit, Alphabit, BlockAlphabit)"
          )
        ; ("--r", Arg.Set_int r, "Drop most significant r bits (Alphabit)")
        ; ("--s", Arg.Set_int s, "Apply test to following s bits (Alphabit)")
        ; ( "--w"
          , Arg.Set_int w
          , "w parameter to BlockAlphabit, must be one of [1,2,4,8,16,32]"
          )
        ; ( "--repeat"
          , Arg.Set_int repeat
          , "How many times to repeat the test (default 1)"
          )
        ]
    in
    let usage =
      Printf.sprintf
        "%s --test test [--index I] [--nbits_log2 NBITS] [--r R] [--s S] [--w \
         W]"
        Sys.argv.(0)
    in
    Arg.parse args (fun s -> raise (Arg.Bad s)) usage ;
    match !test with None -> default () | Some (_, f) -> rerun f gen

  let run_01 name seed f =
    run_cli (gen_01 name seed f) (fun () -> stream_floats seed f)

  let run_buf name seed f =
    run_cli (gen_buf name seed f) (fun () -> stream_buf seed f)
end
