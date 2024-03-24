(** [parse_output all] parses diehard test results, expecting a last line of the form:
  [   diehard_birthdays|   0|       100|     100|0.54283662|  PASSED]
*)
let parse_output all =
  all
  |> String.split_on_char '\n'
  |> List.filter (String.starts_with ~prefix:"#" |> Fun.negate)
  |> List.filter (fun s -> String.contains s '|')
  |> List.fold_left
       (fun (is_assessment, acc) line ->
         match line |> String.split_on_char '|' |> List.rev with
         | "Assessment" :: _ ->
             (true, acc)
         | assessment :: _ when is_assessment ->
             (false, String.trim assessment :: acc)
         | _ ->
             (false, acc)
       )
       (false, [])
  |> snd

(** [check_last_passed all] checks whether the last line parsed contains the field [PASSED].
  It parses a line of the form:
  [   diehard_birthdays|   0|       100|     100|0.54283662|  PASSED]
*)
let check_last_passed all =
  let parsed = parse_output all in
  if parsed = [] then
    Error "No Assesment in output"
  else
    match
      parsed |> List.filter (function "WEAK" | "PASSED" -> false | _ -> true)
    with
    | [] ->
        Ok ()
    | lst ->
        lst |> String.concat "," |> Result.error

let check_results filename =
  In_channel.with_open_text filename @@ fun ch ->
  let all = ch |> In_channel.input_all in
  all |> check_last_passed |> function
  | Ok () ->
      ()
  | Error msg ->
      print_endline all ;
      Printf.eprintf "Test failed: %S\n" msg ;
      flush_all () ;
      exit 1

let () = Arg.parse [] (fun s -> check_results s) "check_dieharder [LOGS...]"
