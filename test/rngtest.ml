open TestU01

let check_results () =
  let threshold = Probdist.Gofw.get_suspectp () in
  Array.combine (Bbattery.get_test_names ()) (Bbattery.get_p_val ())
  |> Array.iter @@ fun (test_name, p_value) ->
  let failure = Printf.sprintf "%s p-value %g" test_name p_value in

  if p_value < threshold || p_value > (1. -. threshold) then
    (* may go away on a rerun *)
    Printf.eprintf "%s\n" failure;
    flush_all ();
    if p_value <= 1e-15 || p_value >= (1. -. 1e-15) then
      (* likely a permanent failure *)
      exit 1

let maken name run n gen =
  List.init n @@ fun i ->
  let i = i + 1 in
  let name = Printf.sprintf "%s_%d" name i in
  name, fun repeat ->
  let rep = Array.make (n + 1) 0 in
  rep.(i) <- repeat;

  run gen rep;

  check_results ()

let make1 name run gen =
  [name, fun _ ->
   run gen;
   check_results ()
  ]

let make_bits name run log2 =
  (* tests require nb >= 512, otherwise they just exit the program,
     and parany will get stuck as it doesn't detect that
   *)
  assert (log2 >= 9);
  let nb = 2. ** (float_of_int log2) in
  make1 name (fun gen -> run gen nb)

let make_bits2 name run ntests log2 =
  assert (log2 >= 9);
  let nb = 2. ** (float_of_int log2) in
  maken name (fun gen -> run gen nb 0 32) ntests

let block_alphabit_w = [|1;2;4;8;16;32|]

let repeat_block_alphabit' gen nb r s rep =
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
let tests gen =
  let open Bbattery in
  [ maken "SmallCrush" repeat_small_crush ntests_small_crush gen
  ; make1 "FIPS-140-2" fips_140_2 gen
  ; make1 "pseudoDIEHARD" pseudo_diehard gen
  ; make_bits "Rabbit" rabbit 25 gen
  ; make_bits2 "BlockAlphabit" repeat_block_alphabit' (ntests_block_alphabit * Array.length block_alphabit_w) 30 gen
  ; maken "Crush" repeat_crush ntests_crush gen
  ; maken "BigCrush" repeat_big_crush ntests_big_crush gen
  ] |> List.concat
  
let () =
  let t = tests (Unif01.create_extern_gen_01 "stdlib" (fun () -> Random.float 1.)) in
  let arg = Sys.argv.(1) in
  if arg = "list" then (t |> List.map fst |> List.iter print_endline)
  else
  1 |> (List.assoc arg t)
(*
  

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
*)
