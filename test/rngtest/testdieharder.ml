let dieharder = "dieharder"

let dieharder_extra_flags =
  [
    "-Y"
  ; "1" (* resolve ambiguity: on a WEAK result rerun with 100 more psamples *)
  ; "-D"
  ; "default"
  ; "-D"
  ; "description"
  ; "-D"
  ; "histogram" (* output format, good for troubleshooting *)
  ; "-g"
  ; "200"
    (* interpret stdin a stream of unsigned 32-bit integers, and use it as the 'RNG' under test *)
  ]

type reliability = Good | Suspect | DoNotUse

let reliability_of_string = function
  | "Good" ->
      Good
  | "Suspect" ->
      Suspect
  | "Do Not Use" ->
      DoNotUse
  | unknown ->
      failwith (Printf.sprintf "Unknown test reliability: %S" unknown)

(** a [dieharder] test description *)
type test = {
    flags: string  (** what flags to use in [dieharder] to run this test *)
  ; name: string  (** the name of this test *)
  ; reliability: reliability  (** {!type:reliability} of this test *)
}

(** [parse_dieharder_list_line acc line] parses a [dieharder -l] output line:
 [  -d 13                             Diehard Squeeze Test              Good]
*)
let parse_dieharder_list_line (started, acc) line =
  let line = String.trim line in
  if String.starts_with ~prefix:"#" line then
    (started, acc)
  else if started then
    ( started
    , match line |> String.split_on_char '\t' with
      | [flags; name; reliability] ->
          {
            flags= String.trim flags
          ; name= String.trim name
          ; reliability= reliability |> String.trim |> reliability_of_string
          }
          :: acc
      | [] | [""] ->
          acc
      | _ ->
          failwith (Printf.sprintf "Cannot parse line: %S" line)
    )
  else if String.starts_with ~prefix:"==" line then
    (true, acc)
  else
    (started, acc)

(** [expand_ntuples] adds [-n] arguments as necessary for some tests.
  Some tests won't run without a [-n] argument.
  Use values similar to what [dieharder-a] would use.
 *)
let expand_ntuples test =
  let make_range nmin nmax flags =
    List.init (nmax - nmin + 1) @@ fun i ->
    {
      test with
      flags= ["-n"; string_of_int (nmin + i); flags] |> String.concat " "
    }
  in
  match String.trim test.flags with
  | "-d 200" as flag ->
      make_range 1 12 flag
  | ("-d 201" | "-d 202") as flag ->
      make_range 2 5 flag
  | "-d 203" as flag ->
      make_range 0 32 flag
  | _other ->
      [test]

let parse_dieharder_list lines =
  lines
  |> String.split_on_char '\n'
  |> List.fold_left parse_dieharder_list_line (false, [])
  |> snd
  |> List.concat_map expand_ntuples

(** [list_tests ()] lists all dieharder tests *)
let list_tests () =
  let open Shexp_process in
  let open Infix in
  run dieharder ["-l"] |- read_all >>| parse_dieharder_list

(** [is_good_test] filters [Good] tests *)
let is_good_test t = t.reliability = Good

(** [print_dune_rule program test] prints a [dune] rule to test [program] using the dieharder [test] *)
let print_dune_rule id program test =
  let nospace s = s |> String.split_on_char ' ' |> String.concat "" in
  let logfile =
    Printf.sprintf "%s%s.log" (Filename.basename program) (nospace test.flags)
  in
  Printf.sprintf
    {|(rule
        (deps (:self ../testdieharder.exe) (:program %s))
        (action
            (with-stdout-to %s
              (run %%{self} --name %s --test %%{program} %s %s)
            )
        )
    )
    (rule
        (deps (:self ../testdieharder.exe) (:log %s))
        (aliases dieharder dieharder_%s dieharder_%s_%d)
        (action (run %%{self} --check %%{log}))
    )
  |}
    program logfile
    (Printf.sprintf "%S" test.name)
    (String.concat " " dieharder_extra_flags)
    test.flags logfile
    (Filename.basename program)
    (Filename.basename program)
    id
  |> print_endline

let list_all programs =
  list_tests ()
  |> Shexp_process.eval
  |> List.filter is_good_test
  |> List.iteri @@ fun id test ->
     programs |> List.iter @@ fun program -> print_dune_rule id program test

(** [parse_output all] parses diehard test results, expecting a last line of the form:
  [   diehard_birthdays|   0|       100|     100|0.54283662|  PASSED]
*)
let parse_output all =
  match
    all
    |> String.split_on_char '\n'
    |> List.rev
    |> List.filter (function "" -> false | _ -> true)
  with
  | [] ->
      None
  | last :: _ -> (
    match last |> String.split_on_char '|' |> List.rev with
    | assesment :: _ ->
        Some (String.trim assesment)
    | _ ->
        None
  )

(** [check_last_passed all] checks whether the last line parsed contains the field [PASSED].
  It parses a line of the form:
  [   diehard_birthdays|   0|       100|     100|0.54283662|  PASSED]
*)
let check_last_passed all =
  match parse_output all with
  | Some "PASSED" ->
      Ok ()
  | Some other ->
      Error other
  | None ->
      Error "No Assesment in output"

let check_results filename =
  In_channel.with_open_text filename @@ fun ch ->
  let all = ch |> In_channel.input_all in
  all |> check_last_passed |> function
  | Ok () ->
      ()
  | Error msg ->
      print_endline all ;
      Printf.eprintf "Test failed: %s\n" msg ;
      flush_all () ;
      exit 1

let run_test = function
  | program :: dieharder_flags ->
      let open Shexp_process in
      let open Infix in
      run program [] |- run dieharder dieharder_flags |> eval
  | [] ->
      failwith "No program given to --test"

let arg_spec =
  Arg.align
    [
      ("--test", Arg.Rest_all run_test, "Runs the specified dieharder test")
    ; ( "--dune"
      , Arg.Rest_all list_all
      , "Generates dune rules to run all the tests"
      )
    ; ("--name", Arg.String ignore, "Test name")
      (* the test name is ignored, but it is useful because it shows up in the processs list *)
    ; ("--check", Arg.String check_results, "Check dieharder results log")
    ]

let () =
  Arg.parse arg_spec
    (fun s -> raise (Arg.Bad s))
    "testdieharder [--test program FLAGS...] [--dune]"
