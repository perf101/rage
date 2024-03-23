open TestU01
open OUnit2

(* TODO: dune dynamimc-include would be better due to caching of runtest?
ounit does some caching, but unclear if it caches on ^C
also you can't see how many you've run or not...
 *)

type t =
{ gen: Unif01.gen 
; name: string
; kind: string
}

let check_results ctx =
  let threshold = Probdist.Gofw.get_suspectp () in
  Array.combine (Bbattery.get_test_names ()) (Bbattery.get_p_val ())
  |> Array.iter @@ fun (test_name, p_value) ->
  if p_value < threshold || p_value > (1. -. threshold) then
    (* may go away on a rerun *)
    let severity = if p_value <= 1e-15 || p_value >= (1. -. 1e-15) then `Error else `Warning in
    logf ctx severity "%s p-value %g" test_name p_value;
    assert_bool "p-value near epsilon" (severity = `Warning)   

(** [redirect_stdout filename] flushes {!val:Stdlib.stdout} and redirects future writes to [filename].
  If [filename] already exists it will be truncated.
  *)
let redirect_stdout target =
   flush stdout;
   let out = Unix.openfile target [Unix.O_CREAT;Unix.O_WRONLY;Unix.O_TRUNC] 0o600 in
   (* this closes Unix.stdout and replaced it with [out].
      [stdout] has a fixed file descriptor number ([1]), this is the only way to reliably change it.
    *)
   Unix.dup2 out Unix.stdout;
   Unix.close out

let with_redirected_stdout f =
  let pid = Unix.getpid () in
  let name, ch = Filename.open_temp_file __MODULE__ (string_of_int pid) in
  Unix.dup2 (Unix.descr_of_out_channel ch) Unix.stdout;
  close_out_noerr ch;
  let finally () = Sys.remove name in
  Fun.protect ~finally @@ fun () ->
  try f ()
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    In_channel.with_open_bin name (fun ch ->
      ch |> In_channel.input_all |> print_endline
    );
    Printexc.raise_with_backtrace e bt

let one run t ctx =
  with_redirected_stdout @@ fun () ->
  run t.gen;
  check_results ctx

let ntests run_repeat n gen =
  List.init n @@ fun i ->
  string_of_int i >:: fun ctx ->
  let repeat = Array.make (n+1) 0 in
  repeat.(i) <- 1;
  one (fun gen -> run_repeat gen repeat) gen ctx

let bits run log2 =
  (* tests require nb >= 512, otherwise they just exit the program,
     and parany will get stuck as it doesn't detect that
   *)
  assert (log2 >= 9);
  let nb = 2. ** (float_of_int log2) in
  one (fun gen -> run gen nb)

let block_alphabit_w = [1;2;4;8;16;32]

let bits2 run n log2 gen =
  assert (log2 >= 9);
  block_alphabit_w |> List.map @@ fun w ->
  string_of_int w >:::
    let nb = 2. ** (float_of_int log2) in
    ntests (fun gen rep -> run gen nb 0 32 rep w) n gen

(* we always use the [repeat] versions that allows chosing individual tests to run,
   so that we can split the jobs across multiple cores  *)
let tests gen =
  let open Bbattery in
  gen.name ^ "_"  ^ gen.kind >:::
  [ "SmallCrush" >::: ntests repeat_small_crush ntests_small_crush gen
  ; "FIPS-140-2" >:: one fips_140_2 gen
  ; "pseudoDIEHARD" >:: one pseudo_diehard gen
  ; "Rabbit" >:: bits rabbit 25 gen
  ; "BlockAlphabit" >::: bits2 repeat_block_alphabit ntests_block_alphabit 30 gen
  ; "Crush" >::: ntests repeat_crush ntests_crush gen
  ; "BigCrush" >::: ntests repeat_big_crush ntests_big_crush gen
  ]
  
(*
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
*)
  
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
