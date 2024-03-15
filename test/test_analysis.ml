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

open Owl

let gen_normal ~sigma mu n =
    n |> gen_uniform |> Array.map (Stats.gaussian_isf ~mu ~sigma)

let gen_random_normal ~sigma mu n =
    Array.init n @@ fun _ -> Stats.gaussian_rvs ~mu ~sigma
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
    let pp_fail ppf ci =
        Fmt.pf ppf "[%g, %g]" ci.low ci.high
    in
    let open Analysis in
    let repeats = 200 in
    (* TODO: first and second order accurate, test over many n... *)
    let allowed_failures = repeats * (5 + (* to allow for errors *) 2)/100 in
    let failures =
        List.init repeats Fun.id
        |> List.filter_map @@ fun _ ->
            let data = distribution value n in
            let ci = compute_ci data in
            validate_ci ci;
            if value < ci.low || value > ci.high then
                Some ci
            else
            None
    in
    let count = List.length failures in
    if count > allowed_failures then
        let name = (List.hd failures).statistic in
        failf "True %s outside of estimated CI: %d failures. Expected %g, got:@, %a" name count value (Fmt.Dump.list pp_fail) failures


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
    List.rev_append (test_ci distribution compute_ci value) @@
    ListLabels.map ~f:(fun n ->
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
    if Array.length data < 100 then skip ()
    else bootstrap_mean data


let () =
  run "Analysis" [
    "tabulate", List.init 200 test_tabulate
   ; "t", List.map test_t_95 [12.706, 1; 2.228, 10; 1.984, 100] 
   ; "gen_normal", List.concat_map test_gen_normal [2;3;5; 10;100;1000]
   ; "mean CI (fixed)", test_ci (gen_normal ~sigma:2.0) normal_ci 5.0
   ; "normal PI (fixed)", test_pi (gen_normal ~sigma:2.0) normal_pi 5.0
   ; "mean CI (random)", test_ci_random (gen_random_normal ~sigma:2.0) normal_ci 0.8
   ; "median CI (fixed)", test_ci (gen_normal ~sigma:2.0) order_ci' 5.0
   ; "order PI (fixed)", test_pi (gen_normal ~sigma:2.0) order_pi' 5.0
   ; "median CI (random)", test_ci_random (gen_random_normal ~sigma:2.0) order_ci' 0.8
   ; "test_quantiles", List.map test_quantiles (List.init 80 (fun n -> n + 6))
   ; "mean CI (fixed, bootstrap)", test_ci (gen_normal ~sigma:2.0) bootstrap_mean 5.0
   ; "mean CI (random, bootstrap)", test_ci_random (gen_random_normal ~sigma:2.0) bootstrap_mean' 0.8
  ]
