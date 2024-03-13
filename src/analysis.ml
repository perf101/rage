open Owl_base

let () =
  (* use a static seed to keep RAGE's results deterministic *)
  Owl_base_stats_prng.init 42


(** [hpt_uni ?alpha ~baseline ~comparison] compares the measurements [comparison] against [baseline].
    
    See "T. Chen et al. Statistical Performance Comparison of Computers. 2012"

    The NULL hypothesis is: "the performance of [baseline] and [comparison] are equivalent"
    The alternative hypothesis is "the performance of [comparison] is larger than [baseline]"
    [alpha] is the significance level and defaults to 0.05 (95% confidence level).

    The caller ([speedup]) will handle the case where we need to compare a slowdown instead.
 *)
let hpt_uni ?alpha ~baseline ~comparison =
  let open Owl in
  (* Wilcoxon Rank-Sum Test, a.k.a. Mann-Whitney U-test. *)
  (Stats.mannwhitneyu ?alpha ~side:Stats.RightSide comparison baseline).reject

let hpt_cross ?alpha ~baseline ~comparison =
  let open Owl in
  let is_significant = Array.map2 (fun baseline comparison ->
      hpt_uni ?alpha ~baseline ~comparison) baseline comparison in
  let baseline =
    Array.map2
      (fun x is -> if is then Stats.median x else 0.)
      baseline is_significant
  in
  let comparison =
    Array.map2
      (fun y is -> if is then Stats.median y else 0.)
      comparison is_significant
  in
  fun gamma ->
    let comparison = Array.map (fun x -> x /. gamma) comparison in
    (Stats.wilcoxon ?alpha ~side:RightSide comparison baseline).reject

(** [speedup ?r ?gamma ~baseline ~comparison] computes the speedup of [comparison] over [baseline]
 * at confidence level [r], starting from value [gamma]. *)
let rec speedup ?r ?(limit = 10.0) ~gamma baseline comparison =
  if gamma >= limit then gamma
  else if hpt_uni ?alpha:r ~comparison:(Array.map (fun x -> x /. gamma) comparison) ~baseline then
    (* [a] significantly outperforms [b] [gamma] times *)
    speedup ?r ~limit ~gamma:(gamma +. 0.01) baseline comparison
  else
    (* We cannot prove that [a] outperforms [b] [gamma] times at [r] confidence level.
     * (Although this might just mean that the performance is identical). *)
    gamma

let speedup ?r ?limit ?(gamma = 1.0) baseline comparison =
	let result = speedup ?r ?limit ~gamma baseline comparison in
	if result = gamma then
    (* it is a slowdown, so compute the 'speedup' the other way and then invert *)
		1. /. (speedup ?r ?limit ~gamma comparison baseline)
	else result

(** [speedup_cross ?r ?gamma a b] computes the speedup of a over b
 * at confidence level [r], starting from value [gamma].
 * Like [speedup], but for multiple benchmarks, e.g. when comparing 2 builds or 2 machines.
 * *)
let speedup_cross ?r ?(limit = 10.0) ?(gamma = 1.0) ~baseline ~comparison =
  if gamma >= limit then gamma
  else
    let hpt = hpt_cross ?alpha:r ~baseline ~comparison in
    let rec loop gamma = if hpt gamma then loop (gamma +. 0.01) else gamma in
    loop gamma

(* Le Boudec, Jean-Yves. Performance Evaluation of Computer and Communication Systems, 2010 *)

(* Appendix A *)
let quantiles n =
    if n <= 5 then None
    else if n <= 70 then
        Some
        [|1,6 ;1,7 ;1,7 ;2,8 ;2,9 ;2,10 ;3,10 ;3,11 ;3,11 ;4,12 ;4,12 ;5,13
        ;5,14 ;5,15 ;6,15 ;6,16 ;6,16 ;7,17 ;7,17 ;8,18 ;8,19 ;8,20 ;9,20 ;9,21
        ;10,21 ;10,22 ;10,22 ;11,23 ;11,23 ;12,24 ;12,24 ;13,25 ;13,26 ;13,27
        ;14,27 ;14,28 ;15,28 ;15,29 ;16,29 ;16,30 ;16,30 ;17,31 ;17,31 ;18,32
        ;18,32 ;19,33 ;19,34 ;19,35 ;20,35 ;20,36 ;21,36 ;21,37 ;22,37 ;22,38
        ;23,39 ;23,39 ;24,40 ;24,40 ;24,40 ;25,41 ;25,41 ;26,42 ;26,43 ;26,44
        ;27,44|].(n-5)
    else
        let n = float n in
        let sqrt_n_98 = 0.98 *. iqrt n
        and half_n = 0.5 *. n in
        Some (
            half_n -. sqrt_n_98 |> Float.floor |> int_of_float,
            half_n +. 1.0 +. sqrt_n_98 |> Float.ceil |> int_of_float
        )

let confidence_median data =
    let n = Array.length data in
    match quantiles n with
    | None -> (* for very small [n], no confidence interval is possible *)
    | Some (j, k) ->
        let order = Stats.sort ~inc:true data in
        order.(j), order.(k)



let bootstrap_gen ?(r0 = 25) ?(gamma = 0.95) f t xs =
  let r = (Float.ceil (float (2 * r0) /. (1. -. gamma)) |> int_of_float) - 1 in
  let boot_samples = Array.init r (fun _ -> xs |> f |> t) in
  Array.sort Float.compare boot_samples ;
  (* percentile bootstrap estimate *)
  (boot_samples.(r0), t xs, boot_samples.(r + 1 - r0))

let sample xs = Stats.sample xs (Array.length xs)

let sample2 (xs, ys) = (sample xs, sample ys)

(** [bootstrap ?r0 ?gamma t xs] computes the confidence interval at level [gamma] for the
 * statistic [t]. [xs] are samples from an iid sequence, and [r0] is the algorithm's accuracy
 * parameter. Does not require the distribution to be normal.
 *)
let bootstrap ?gamma t xs = bootstrap_gen ?gamma sample t xs

let bootstrap_mean ?gamma = bootstrap ?gamma Stats.mean

(* T. Kalibera, R. Jones. Quantifying Performance Changes with Effect Size Confidence Intervals. 2012 *)

(** [bootstrap_ratio ?gamma old_ys new_ys] computes the bootstrap confidence interval at level [gamma]
 * for the ratio of means of two systems *)
let bootstrap_ratio ?gamma baseline comparison =
  let ratio (ns, os) = Stats.mean ns /. Stats.mean os in
  bootstrap_gen ?gamma sample2 ratio (comparison, baseline)
