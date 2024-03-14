open Alcotest.V1

open Analysis

let test_f x =
    if x < 1 then invalid_arg "out of range";
    x*2+1

let test_tabulate =
    let tabulated = tabulate 100 test_f in
    fun i ->
        test_case (string_of_int i) `Quick @@ fun () ->
        let i = i + 1 in
        check' ~msg:"tabulated" ~expected:(test_f i) ~actual:(tabulated i) int

let test_t_95 (expected, df) =
  test_case (string_of_int df) `Quick @@ fun () ->
  check' ~msg:"t_95" ~expected ~actual:(t_95 df) (float 0.001)

let () =
  run "Analysis" [
    "tabulate", List.init 200 test_tabulate
   ; "t", List.map test_t_95 [12.706, 1; 2.228, 10; 1.984, 100] 
  ]
