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

let gen_uniform n =
    let delta = 1. /. float_of_int (n + 1) in
    (* uniformly distributed on [(0, 1)] *)
    Array.init n @@ fun i ->
    delta *. (float_of_int @@ i + 1)

let gen_normal ~sigma mu n =
    n |> gen_uniform |> Array.map (Owl.Stats.gaussian_isf ~mu ~sigma)

let gen_random_normal ~sigma mu n =
    Array.init n @@ fun _ -> Owl_stats.gaussian_rvs ~mu ~sigma
    (*
      We could also use a RNG, but we want to use deterministic and more accurate values in the test instead of:
      mu +. sigma *. Owl_stats_prng.rand_gaussian ()
      Owl_stats.gaussian_rvs ~mu ~sigma 
    *)

let validate_ci ci =
    if ci.low > ci.high then
        failf "Confidence interval endpoints wrong way around: %f, %f" ci.low ci.high;
    if ci.value < ci.low || ci.value > ci.high then
        failf "Reported %s doesn't satisfy %g ∈ [%g, %g]" ci.statistic ci.value ci.low ci.high

let test_ci_outside distribution compute_ci value n () =
    let open Analysis in
    let bad = ref 0 in
    let repeats = 500 in
    let allowed_failures = repeats * 95/100 in
    for i = 1 to repeats do
        let data = distribution value n in
        let ci = compute_ci data in
        validate_ci ci;
        if value < ci.low || value > ci.high then
            incr bad;
        if !bad > allowed_failures then
            failf "True %s = %g outside of estimated CI [%g, %g]" ci.statistic value ci.low ci.high
    done

let test_accuracy distribution compute_ci value () =
    let data = distribution value 1000 in
    let ci = compute_ci data in
    validate_ci ci;
    let value' = match ci.statistic with
    | "mean" -> Owl.Stats.mean data
    | "median" -> Owl.Stats.median data
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
        if n >= 10000 && ratio > 0.999 then
            failf "Prediction interval too wide, %.2f%% of the %d data is inside: [%g, %g]" (ratio*.100.) (Array.length data) pi.low pi.high

let test_pi_count distribution compute_pi value =
    let large_data = distribution value 10000 in
    fun n () ->
        let data = distribution value n in
        let pi = compute_pi data in
        validate_ci pi;
        check_pi pi n data;
        check_pi pi n large_data

let test_ci distribution compute_ci value =
    test_case "accuracy" `Quick (test_accuracy distribution compute_ci value)
    :: test_case "CI width" `Quick (test_ci_vs_pi distribution compute_ci value)
    :: ListLabels.map ~f:(fun n ->
         let name = Printf.sprintf "CI outside (%d)" n in
         test_case name `Slow (test_ci_outside distribution compute_ci value n)
    )
    [ 1000; 100; 10; 5; 3; 2 ]    

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
        if (Owl.Stats.jb_test data).reject then
            failf "Generated data doesn't follow a normal distribution: %a" Fmt.(array float) data;
      )
    ; test_case "normal distribution with mu" `Quick (fun () ->
        if (Owl.Stats.z_test data ~mu ~sigma).reject then
            failf "Generated data doesn't follow a normal distribution with mu=%g: %a" mu Fmt.(array float) data;
      )
    ; test_case "normal distribution with variance" `Quick (fun () ->
        if (Owl.Stats.var_test data ~variance:(sigma *. sigma)).reject then
            failf "Generated data doesn't follow a normal distribution with stdev=%g: %a" sigma Fmt.(array float) data;
      )
    ]


let () =
  run "Analysis" [
    "tabulate", List.init 200 test_tabulate
   ; "t", List.map test_t_95 [12.706, 1; 2.228, 10; 1.984, 100] 
   ; "gen_normal", List.concat_map test_gen_normal [2;3;5; 10;100;1000]
   ; "mean CI (fixed)", test_ci (gen_normal ~sigma:2.0) mean_ci 5.0
   ; "mean PI (fixed)", test_pi (gen_normal ~sigma:2.0) mean_pi 5.0
   ; "mean CI (random)", test_ci (gen_random_normal ~sigma:2.0) mean_ci 5.0
  ]
