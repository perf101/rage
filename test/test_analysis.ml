open Alcotest.V1

open Analysis
open Datagen

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

open Owl

let gen_normal ~sigma mu n = Normal.low_discrepancy ~mu ~sigma n

let gen_random_normal ~sigma mu n = Normal.rand1 ~mu ~sigma n
let gen_random_normal' ~sigma mu n = Normal.rand2 ~mu ~sigma n
    (*
      We could also use [mu +. sigma *. Owl_stats_prng.rand_gaussian ()], but that uses the Ziggurat algorithm, which has some known flaws
    *)

let validate_ci ci =
    if ci.low > ci.high then
        failf "Confidence interval endpoints wrong way around: %f, %f" ci.low ci.high;
    if ci.value < ci.low || ci.value > ci.high then
        failf "Reported %s doesn't satisfy %g ∈ [%g, %g]" ci.statistic ci.value ci.low ci.high

let count f l = List.fold_left (fun acc x -> if f x then acc + 1 else acc) 0 l
let range a b = Array.init (b-a) @@ fun i -> a + i

let compute_ci_accuracy ?(repeats=200) distribution compute_ci value n_min n_max =
    range n_min n_max
    |> Array.map @@ fun n ->
    let n' = float_of_int n in
    let to_ratio i = float_of_int i /. float_of_int n in
    let ci =
        List.init repeats @@ fun _ ->
        let ci = distribution value n |> compute_ci in
        validate_ci ci;
        ci
    in
    let lo_noncoverage = count (fun ci -> value < ci.low) ci |> to_ratio
    and hi_noncoverage = count (fun ci -> value > ci.high) ci |> to_ratio
    in
    (lo_noncoverage, n'), (hi_noncoverage, n')

module M = Owl.Dense.Matrix.D

let of_array a = M.of_array a 1 (Array.length a)

let fit values bigo =
    let y = Array.map fst values |> of_array
    and x = Array.map (fun (_, n) -> bigo n) values |> of_array
    in
    (* fit y = a + b * x *)
    let a, h = Owl.Linalg.D.linreg x y in
    a, h, a > alpha

let fit_first_order values =
    let a, h, reject = fit values (fun n -> 1. /. sqrt n) in
    if reject then
        failf "CI is not first order accurate: non-coverage = %g + %g / sqrt n" a h

let fit_second_order values =
    let a, h, reject = fit values (fun n -> 1. /. n) in
    if reject then
        failf "CI is not second order accurate: non-coverage = %g + %g / n" a h

let test_ci_accuracy ?repeats distribution compute_ci fit_test value  =
    let lo_noncoverage, hi_noncoverage = compute_ci_accuracy ?repeats distribution compute_ci value 6 50 |> Array.split in
    [ test_case "interval low" `Quick (fun () -> fit_test  lo_noncoverage)
    ; test_case "interval high" `Quick (fun () -> fit_test  hi_noncoverage)
    ]

let test_accuracy distribution compute_ci value () =
    let data = distribution value 1000 in
    let ci = compute_ci data in
    validate_ci ci;
    let value' = match ci.statistic with
    | "mean" -> Stats.mean data
    | "median" -> Stats.median data
    | s -> failf "Unknown statistic %s" s
    in
    let allowed_error = abs_float (value' -. value) +. 0.0001 in
    check' ~msg:"Estimated vs actual" ~expected:value ~actual:ci.value (float allowed_error)

let test_ci_vs_pi distribution compute_ci value () =
    let data = distribution value 1000 in
    let ci = compute_ci data in
    validate_ci ci;
    let inside =
        Array.fold_left (fun acc e ->
            if ci.low <= e && e <= ci.high then acc + 1 else acc
        ) 0 data
    in
    let ratio = float_of_int inside /. float_of_int (Array.length data) in
    if ratio > 0.95 then
        (* the CI is meant to be the confidence interval of the statistic,
           and NOT a prediction interval for the values *)
            failf "Confidence interval too wide, %.1f%% of the data is inside: [%g, %g]" (ratio*.100.) ci.low ci.high

let check_pi pi n data =
        let inside =
            Array.fold_left (fun acc e ->
                if pi.low <= e && e <= pi.high then acc + 1 else acc
            ) 0 data
        in
        let ratio = float_of_int inside /. float_of_int (Array.length data) in
        let percentage = ratio *. 100. in
        if (n < 20 && percentage < 90.)
            (* would be 95., but we get 94.46 sometimes... *)
           || percentage < 94. then
            failf "Prediction interval too narrow, only %.2f%% of the %d data is inside: [%g, %g]" percentage (Array.length data) pi.low pi.high;
        if n >= 1000 && ratio > 0.99 then
            failf "Prediction interval too wide, %.2f%% of the %d data is inside: [%g, %g]" (ratio*.100.) (Array.length data) pi.low pi.high

let test_pi_count distribution compute_pi value =
    let large_data = distribution value 1000 in
    fun n () ->
        let data = distribution value n in
        let pi = compute_pi data in
        validate_ci pi;
        check_pi pi n data;
        check_pi pi n large_data

let test_ci distribution compute_ci value =
    [test_case "accuracy" `Quick (test_accuracy distribution compute_ci value)
    ; test_case "CI width" `Quick (test_ci_vs_pi distribution compute_ci value)
    ]

let test_ci_random distribution compute_ci value =
    List.concat
    [ test_ci distribution compute_ci value
    ; test_ci_accuracy distribution compute_ci fit_first_order value
(*   ; test_ci_accuracy distribution compute_ci fit_second_order value*)
    ]

let test_pi distribution compute_pi value =
    test_case "accuracy" `Quick (test_accuracy distribution compute_pi value)
    :: ListLabels.map ~f:(fun n ->
         let name = Printf.sprintf "PI outside (%d)" n in
         test_case name `Slow (test_pi_count distribution compute_pi value n)
    )
    [ 1000; 100; 10; 5; 3; 2 ]    

let test_gen_normal n =
    let mu = 0.0 and sigma = 1.0 in
    let data = gen_normal ~sigma mu n in
    [ test_case "normal distribution" `Quick (fun () ->
        if (Stats.jb_test data).reject then
            failf "Generated data doesn't follow a normal distribution: %a" Fmt.(array float) data;
      )
    ; test_case "normal distribution with mu" `Quick (fun () ->
        if (Stats.z_test data ~mu ~sigma).reject then
            failf "Generated data doesn't follow a normal distribution with mu=%g: %a" mu Fmt.(array float) data;
      )
    ; test_case "normal distribution with variance" `Quick (fun () ->
        if (Stats.var_test data ~variance:(sigma *. sigma)).reject then
            failf "Generated data doesn't follow a normal distribution with stdev=%g: %a" sigma Fmt.(array float) data;
      )
    ]

let test_quantiles n =  
  test_case (string_of_int n) `Quick @@ fun () ->
  match quantile_confidence n with
  | None -> fail "no confidence interval returned"
  | Some (j, k) ->
    let v = Maths.bdtr (k-1) n 0.5 -. Maths.bdtr (j-1) n 0.5 in
    if v < alpha then
      failf "Expected: B(k-1) - B(j-1) >= alpha, but got %g < alpha" v

let order_ci' data =
    match order_ci data with
    | None -> skip ()
    | Some r -> r

let order_pi' data =
    match order_pi data with
    | None -> skip ()
    | Some r -> r

let bootstrap_mean' data =
    (* TODO: BCa? *)
    bootstrap_mean data


let () =
  run "Analysis" [
    "tabulate", List.init 200 test_tabulate
   ; "t", List.map test_t_95 [12.706, 1; 2.228, 10; 1.984, 100] 
   ; "gen_normal", List.concat_map test_gen_normal [2;3;5; 10;100;1000]
   ; "mean CI (fixed)", test_ci (gen_normal ~sigma:2.0) normal_ci 5.0
   ; "normal PI (fixed)", test_pi (gen_normal ~sigma:2.0) normal_pi 5.0
   ; "mean CI (random)", test_ci_random (gen_random_normal ~sigma:2.0) normal_ci 0.8
   ; "mean CI (random')", test_ci_random (gen_random_normal' ~sigma:2.0) normal_ci 0.8
   ; "median CI (fixed)", test_ci (gen_normal ~sigma:2.0) order_ci' 5.0
   ; "order PI (fixed)", test_pi (gen_normal ~sigma:2.0) order_pi' 5.0
   ; "median CI (random)", test_ci_random (gen_random_normal ~sigma:2.0) order_ci' 0.8
   ; "test_quantiles", List.map test_quantiles (List.init 80 (fun n -> n + 6))
   ; "mean CI (fixed, bootstrap)", test_ci (gen_normal ~sigma:2.0) bootstrap_mean 5.0
   ; "mean CI (random, bootstrap)", test_ci_random (gen_random_normal ~sigma:2.0) bootstrap_mean' 0.8
   ; "mean CI (random', bootstrap)", test_ci_random (gen_random_normal' ~sigma:2.0) bootstrap_mean' 0.8
  ]
