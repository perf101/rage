let count p lst = List.fold_left (fun acc e -> if p e then acc + 1 else acc) 0 lst

(* [tqdm]-like progress bar *)
let line name total =
  let open Progress in
  let count1 segment =
    Line.using (fun _ -> 1) (segment total)
  in
  let is_warning = function
  | Error (Logs.Warning, _ ) -> true
  | _ -> false
  in
  let is_error = function
  | Error (Logs.Error, _) -> true
  | _ -> false
  in
  Line.(list
  [ spinner ()
  ; (rpad 14 (const name))
  ; count1 percentage_of
  ; count1 (bar ~style:`UTF8)
  ; (lpad 7 (ticker_to total))
  ; brackets (list ~sep:(const ", ")
   [ elapsed () ++ const "<" ++ count1 eta
   ; using (count Result.is_ok) (const "ok=" ++ sum ~width:3 ())
   ; using (count is_warning) (const "warn=" ++ sum ~width:3 ())
   ; using (count is_error) (const "err=" ++ sum ~width:3 ())
   ])
  ])

(* multiline here is buggy, it leaves some extra uncleared lines on the screen,
   and it then also displays the wrong progressbar alongside wrong name,
   also eta is way too high on lines that were not run yet
   use a single line instead
 *)
let lines tests =
  tests |> List.map (fun (name, total) -> line name total) |> Progress.Multi.lines

type ('a, 'b, 'c) t =
{ name: string
; run: 'a -> ('b, 'c) result list
}

let cpu = Cpu.numcores ()

(** [redirect_stdout_stderr filename] flushed {!val:Stdlib.stdout}, {!val:Stdlib.stderr} and redirects future writes to [filename]. If [filename] already exists it will be truncated.
 *)
let redirect_stdout_stderr target =
  flush stdout;
  flush stderr;
  let out = Unix.openfile target [Unix.O_CREAT;Unix.O_WRONLY;Unix.O_TRUNC] 0o600 in
  (* this closes Unix.stdout and replaced it with [out].
     [stdout] has a fixed file descriptor number ([1]), this is the only way to reliably change it.
   *)
  Unix.dup2 out Unix.stdout;
  Unix.dup2 out Unix.stderr;
  Unix.close out

let parallel ~describe_input tests_and_inputs =
  Logs.set_reporter (Progress.logs_reporter ());
  let desc = tests_and_inputs |> List.map (fun (test, input) -> test.name, List.length input) in
  flush_all ();
  let config = Progress.Config.v ~hide_cursor:false () in
  Progress.with_reporters ~config (lines desc) @@ fun report ->
    let tests_and_inputs =
      tests_and_inputs
      |> List.combine report
      |> List.concat_map (fun (report, (test, inputs)) -> inputs |> List.map @@ fun input -> report, (test, input))
      |> Array.of_list
    in
    let process_result_main acc (result, filename, index) =
      result |> List.iter (function
        | Ok _ -> ()
        | Error (level, msg) ->
          Logs.msg level (fun m -> m "%s: %s" filename msg)
      );
      let report, _ = tests_and_inputs.(index) in
      report result;
      result :: acc
    in
    let child_process index =
      let _, (test, input) = tests_and_inputs.(index) in
      let filename = Printf.sprintf "%s_%s.out" test.name (describe_input input) in
      redirect_stdout_stderr filename;
      test.run input, filename, index
    in
    List.init (Array.length tests_and_inputs) Fun.id
    |> Parany.Parmap.parfold cpu child_process process_result_main []
    |> List.concat
