let count p lst = List.fold_left (fun acc e -> if p e then acc + 1 else acc) 0 lst

(* [tqdm]-like progress bar *)
let line name total =
  let open Progress in
  let count1 segment =
    Line.using (fun _ -> 1) (segment total)
  in
  Line.(list
  [ spinner ()
  ; const name
  ; count1 percentage_of
  ; count1 (bar ~style:`UTF8)
  ; ticker_to total
  ; brackets (list
   [ elapsed ()
   ; const "<"
   ; count1 eta
   ; const ", "
   ; using (count Result.is_ok) (const "ok=" ++ sum ~width:4 ())
   ; const ", "
   ; using (count Result.is_error) (const "errors=" ++ sum ~width:4 ())
   ])
  ])

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
  let child_process (report, (test, input)) =
    let filename = Printf.sprintf "%s_%s" test.name (describe_input input) in
    redirect_stdout_stderr filename;
    test.run input, filename, report
  in

  let process_result_main acc (result, filename, report) =
    result |> List.iter (function
      | Ok _ -> ()
      | Error (level, msg) ->
        Logs.msg level (fun m -> m "%s: %s" filename msg)
    );
    report result;
    result :: acc
  in

  Logs.set_reporter (Progress.logs_reporter ());
  let desc = tests_and_inputs |> List.map (fun (test, input) -> test.name, List.length input) in
  Progress.with_reporters (lines desc) @@ fun report ->
    tests_and_inputs
    |> List.concat_map (fun (test, inputs) -> inputs |> List.map @@ fun input -> test, input)
    |> List.combine report
    |> Parany.Parmap.parfold cpu child_process process_result_main []
    |> List.concat
