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
  ; "-k"
  ; "1" (* use more accurate KS test *)
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

(** [parse_dieharder_list output] parses the output of [dieharder -l]. *)
let parse_dieharder_list output =
  output
  |> String.split_on_char '\n'
  |> List.fold_left parse_dieharder_list_line (false, [])
  |> snd
  |> List.concat_map expand_ntuples

(** [list_tests ()] lists all dieharder tests *)
let list_tests () =
  let ch = Unix.open_process_in "dieharder -l" in
  let all = ch |> In_channel.input_all in
  close_in ch ;
  all |> parse_dieharder_list

(** [is_good_test] filters [Good] tests *)
let is_good_test t = t.reliability = Good

let logfile kind program flags =
  let nospace s = s |> String.split_on_char ' ' |> String.concat "" in
  Printf.sprintf "%s_%s%s.log" kind (Filename.basename program) (nospace flags)

(** [print_dune_rule program test] prints a [dune] rule to test [program] using the dieharder [test] *)
let print_dune_rule id program test =
  let logfile = logfile "dieharder" program test.flags in
  Printf.sprintf
    {|
      ; %s
      (rule
        (deps (:program %s))
        (action
            (with-stdout-to %s
              (bash "%%{program} | dieharder %s %s")
            )
        )
      )
      (rule
        (deps (:check ../check_dieharder.exe) (:log %s))
        (aliases rngtest dieharder dieharder_%s dieharder_%s_%d)
        (action (run %%{check} %%{log}))
      )
  |}
    test.name program logfile
    (String.concat " " dieharder_extra_flags)
    test.flags logfile
    (Filename.basename program)
    (Filename.basename program)
    id
  |> print_endline

let list_all programs =
  list_tests ()
  |> List.filter is_good_test
  |> List.iteri @@ fun id test ->
     programs |> List.iter @@ fun program -> print_dune_rule id program test

let list_testu01 programs =
  programs
  |> List.iter @@ fun program ->
     Rngtest.RunTestU01.tests
     |> List.iter @@ fun (name, (args, _)) ->
        args
        |> List.iter @@ fun arg ->
           let arg = Printf.sprintf "--test %s %s" name arg in
           Printf.printf
             {|(rule
                (aliases rngtest testu01 testu01_%s)
                (deps (:program %s))
                (action
                  (with-stdout-to %s
                    (run %%{program} %s)
                  )
                )
              )
             |}
             (Filename.basename program) program (logfile "testu01" program arg) arg

let list_practrand programs =
  programs
  |> List.iter @@ fun program ->
  let log = logfile "practrand" program "" in
  Printf.printf {|
    (rule
      (aliases rngtest practrand practrand_%s)
      (deps (:program %s))
      (action
        (with-stdout-to %s
          (bash "%%{program} | practrand-RNG_test stdin32 -multithreaded -tlmax 512GB")
        )
      )
    )
  |} (Filename.basename program) program log

let () =
  let programs = Sys.argv |> Array.to_list |> List.tl in
  list_all programs ; list_testu01 programs; list_practrand programs
