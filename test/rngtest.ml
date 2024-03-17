open TestU01

(** [redirect_stdout filename] flushed {!val:Stdlib.stdout} and redirects future {!val:Stdlib.stdout} and {!val:Unix.stdout} writes to [filename]. If [filename] already exists it will be truncated.
 *)
let redirect_stdout target =
  flush stdout;
  let out = Unix.openfile target [Unix.O_CREAT;Unix.O_WRONLY;Unix.O_TRUNC] 0o600 in
  (* this closes Unix.stdout and replaced it with [out].
     [stdout] has a fixed file descriptor number ([1]), this is the only way to reliably change it.
   *)
  Unix.dup2 out Unix.stdout;
  Unix.close out

type test =
{ battery: string
; run: Unif01.gen -> int array -> unit
; njobs: int
}

type gen = 
{ gen: Unif01.gen
; name: string  
; kind: string
}

type failure =
{ filename: string
; test_name: string
; p_value: float
}

let print_failure_details f =
  In_channel.with_open_bin f.filename @@ fun ch ->
  ch |> In_channel.input_all |> prerr_endline

let print_failure_summary f =
  Printf.eprintf "%s: %s p-value %g\n" f.filename f.test_name f.p_value

(** [run_testu01 ?repeat battery gen test_index] runs the [test_index]th test from the test [battery]
  on the random number generator [gen].

  @params repeat how many times to repeat the test (default: 1, can be >1 if you want to investigate failure probabilities)
  @params battery a testu01 test battery
  @params gen the generator to test
  @params test_index a number [1 <= test_index <= battery.jobs]

  @returns a list of {!type:failure} failures. Note that a single test may internally run multiple tests and can return more than 1 failure
 *)
let run_testu01_i ?(repeat=1) test gen test_index =
  if test_index < 1 || test_index > test.njobs then
    invalid_arg (Printf.sprintf "test_index: %d" test_index);

  let filename = Printf.sprintf "%s_%s_%s_%d.out" gen.name gen.kind test.battery test_index in
  redirect_stdout filename;

  let rep = Array.make (test.njobs+1) 0 in
  rep.(test_index) <- repeat;

  test.run gen.gen rep;
  flush_all ();

  let threshold = Probdist.Gofw.get_suspectp () in
  let tests = Array.combine (Bbattery.get_test_names ()) (Bbattery.get_p_val ()) in
  tests |> Array.to_seq |> Seq.filter (fun (_, p) -> p < threshold || p > (1. -. threshold))
  |> Seq.map (fun (test_name, p_value) -> {filename;test_name;p_value})
  |> List.of_seq

let cores = Cpu.numcores ()

let had_failures = ref 0

let run_testu01 test gen =
  Printf.printf "Running %s (%d jobs) …%!" test.battery test.njobs;
  let t0 = Unix.gettimeofday () in
  let failures =
    List.init test.njobs (fun i -> i + 1)
    |> Parany.Parmap.parmap cores (run_testu01_i test gen)
    |> List.concat 
  in
  let t1 = Unix.gettimeofday () in
  Printf.printf " completed in %.1fs with %d failures\n%!" (t1 -. t0) (List.length failures);
  let nfailures = List.length failures in
  if nfailures > 0 then begin
    failures |> List.iter print_failure_details;
    Printf.eprintf "Failed randomness tests:\n";
    failures |> List.iter print_failure_summary;
    Printf.eprintf "There were %d failed tests\n" nfailures;
    flush_all ();
    incr had_failures
  end

(** The {!module:TestU01} test batteries *)
let batteries =
  let vn battery run njobs =
    { battery; run; njobs}
  in
  let v1 battery run1 =
    (* these have 1 job, but multiple tests inside *)
    vn battery (fun gen _ -> run1 gen) 1
  in
  let vbits battery runb log2 =
    (* tests require nb >= 512, otherwise they just exit the program,
       and parany will get stuck as it doesn't detect that
     *)
    assert (log2 >= 9);
    let nb = 2. ** (float_of_int log2) in
    v1 battery (fun gen -> runb gen nb)
  in
  let vbits2 battery runb ntests log2 =
    assert (log2 >= 9);
    let nb = 2. ** (float_of_int log2) in
    vn battery (fun gen -> runb gen nb 0 32) ntests
  in
  let block_alphabit_w = [|1;2;4;8;16;32|] in
  let repeat_block_alphabit gen nb r s rep =
    (* we have n jobs to run, combined with a different w each time,
       the high-level runner puts everything together:
       n runs with w=1, then n runs with w=2, and so on...
     *)
    rep |> Array.iteri @@ fun i count ->
    (* was indexed from 1 *)
    let i = i - 1 in
    let n = Bbattery.ntests_block_alphabit in
    let rep = Array.make (1+n) 0 in
    rep.(1 + i mod n) <- count;
    let w = block_alphabit_w.(i / n) in
    Bbattery.repeat_block_alphabit gen nb r s rep w
  in
  (* we always use the [repeat] versions that allows chosing individual tests to run,
     so that we can split the jobs across multiple cores  *)
  [ vn "SmallCrush" Bbattery.repeat_small_crush Bbattery.ntests_small_crush
  ; v1 "FIPS-140-2" Bbattery.fips_140_2
  ; v1 "PseudoDIEHARD" Bbattery.pseudo_diehard
  ; vbits "Rabbit" Bbattery.rabbit 25
  ; vbits2 "Alphabit" Bbattery.repeat_alphabit Bbattery.ntests_alphabit 30
  ; vbits2 "BlockAlphabit" repeat_block_alphabit (Bbattery.ntests_block_alphabit * Array.length block_alphabit_w) 30
  ; vn "Crush" Bbattery.repeat_crush Bbattery.ntests_crush
  ; vn "BigCrush" Bbattery.repeat_big_crush Bbattery.ntests_big_crush
  ]

let run_all gen =
  batteries |> List.iter @@ fun battery ->
  run_testu01 battery gen

let int32 name f =
  { gen = Unif01.create_extern_gen_int32 name f
  ; name
  ; kind = "int32"
  }

let f01 name f =
  { gen = Unif01.create_extern_gen_01 name f
  ; name
  ; kind = "float[0,1)"
  }

let float name f =
  (* TestU01 doesn't accept 1 as a random value, need to be [0, 1),
     but both Owl and OCaml's RNG has inclusive bounds
   *)
  let rec wrap () =
    (* we could multiply with (Float.pred 1), but that may affect the bit patterns *)
    match f () with
    | 1. -> wrap ()
    | r -> r
  in
  { (f01 name wrap) with kind = "float[0,1]" }
