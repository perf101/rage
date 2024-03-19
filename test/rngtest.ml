open TestU01

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
  Printf.eprintf "%s p-value %g\n" f.test_name f.p_value

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

  let rep = Array.make (test.njobs+1) 0 in
  rep.(test_index) <- repeat;

  test.run gen.gen rep;

  let threshold = Probdist.Gofw.get_suspectp () in
  let tests = Array.combine (Bbattery.get_test_names ()) (Bbattery.get_p_val ()) in
  tests |> Array.to_list |> List.map @@ fun (test_name, p_value) ->
  let failure = Printf.sprintf "%s p-value %g" test_name p_value in
  if p_value < 1e-15 || p_value > (1. -. 1e-15) then
    (* very unlikely to go away on a rerun *)
    Error (Logs.Error, failure)
  else if p_value < threshold || p_value > (1. -. threshold) then
    (* may go away on a rerun *)
    Error (Logs.Warning, failure)
  else
   Ok ()

let run_testu01_common ?repeat gen tests =
  let describe_input input =
    Printf.sprintf "%d_%s" input gen.kind
  in
  let errors = 
    tests |> List.map (fun (test, input) ->
      let name = test.battery in
      Parview.{ name; run = (run_testu01_i ?repeat test gen) }, input
    )
    |> Parview.parallel ~describe_input
    |> List.filter (function Error (Logs.Error, _) -> true | _ -> false)
    |> List.length
  in
  if errors > 0 then begin
    Printf.eprintf "There were %d errors\n%!" errors;
    exit 1
  end

let run_testu01 ?repeat gen tests =
  tests
  |> List.map (fun battery -> battery, List.init battery.njobs (fun i -> i+1))
  |> run_testu01_common ?repeat gen
  
(** The {!module:TestU01} test batteries *)
let vn battery run njobs =
  { battery; run; njobs}
let v1 battery run1 =
  (* these have 1 job, but multiple tests inside *)
  vn battery (fun gen _ -> run1 gen) 1
let vbits battery runb log2 =
  (* tests require nb >= 512, otherwise they just exit the program,
     and parany will get stuck as it doesn't detect that
   *)
  assert (log2 >= 9);
  let nb = 2. ** (float_of_int log2) in
  v1 battery (fun gen -> runb gen nb)
let vbits2 battery runb ntests log2 =
  assert (log2 >= 9);
  let nb = 2. ** (float_of_int log2) in
  vn battery (fun gen -> runb gen nb 0 32) ntests
let block_alphabit_w = [|1;2;4;8;16;32|]
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


(* we always use the [repeat] versions that allows chosing individual tests to run,
   so that we can split the jobs across multiple cores  *)
let small_crush = vn "SmallCrush" Bbattery.repeat_small_crush Bbattery.ntests_small_crush
let fips_140_2 = v1 "FIPS-140-2" Bbattery.fips_140_2
let pseudoDIEHARD = v1 "PseudoDIEHARD" Bbattery.pseudo_diehard
let rabbit = vbits "Rabbit" Bbattery.rabbit 25
let alphabit = vbits2 "Alphabit" Bbattery.repeat_alphabit Bbattery.ntests_alphabit 30
let block_alphabit = vbits2 "BlockAlphabit" repeat_block_alphabit (Bbattery.ntests_block_alphabit * Array.length block_alphabit_w) 30
let crush = vn "Crush" Bbattery.repeat_crush Bbattery.ntests_crush
let big_crush = vn "BigCrush" Bbattery.repeat_big_crush Bbattery.ntests_big_crush

let batteries = [
  small_crush; fips_140_2; pseudoDIEHARD; rabbit; alphabit; block_alphabit; crush; big_crush      
]

let run_all gen =
  batteries |> run_testu01 gen

(** a custom battery of tests based on tests that have been observed to fail in any of the OCaml generators that we tested. *)
let run_custom gen repeat =
    [
     crush, [88]
    ; big_crush, [61; 64; 83; 94; 95; 98; 103; 106]
    ; pseudoDIEHARD, [1]
    ; block_alphabit, [18]
    ]
  |> run_testu01_common ~repeat gen

(* small subset of run_custom *)
let run_custom_small gen repeat =
    [
     crush, [88]
    ; pseudoDIEHARD, [1]
    ; block_alphabit, [18]
    ]
  |> run_testu01_common ~repeat gen
  
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
