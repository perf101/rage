let run_test = function
  | program :: dieharder_flags ->
      run program [] |- run dieharder dieharder_flags |> eval
  | [] ->
      failwith "No program given to --test"

let () =
  Sys.argv |> Array.to_list |> List.tl |> run_test
