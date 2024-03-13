open Alcotest.V1

open Analysis
open Owl

let test_t_95 (expected, df) =
  test_case (string_of_int df) `Quick @@ fun () ->
  check' ~msg:"t_95" ~expected ~actual:(t_95 df) (float 0.001)

let test_quantiles n =  
  test_case (string_of_int n) `Quick @@ fun () ->
  match quantile_confidence n with
  | None -> fail "no confidence interval returned"
  | Some (j, k) ->
    let v = Maths.bdtr (k-1) n 0.5 -. Maths.bdtr (j-1) n 0.5 in
    if v < alpha then
      failf "Expected: B(k-1) - B(j-1) >= alpha, but got %g < alpha" v

let () =
  run "Analysis" [
    "t", List.map test_t_95 [12.706, 1; 2.228, 10; 1.984, 100] 
  ; "quantiles", List.map test_quantiles (List.init 80 (fun n -> n + 6))
  ]
