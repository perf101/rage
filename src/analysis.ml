open Owl_base

let () =
  (* use a static seed to keep RAGE's results deterministic *)
  Owl_base_stats_prng.init 42

(* T. Chen et al. Statistical Performance Comparison of Computers. 2012 *)
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
let rec speedup ?r ?(limit = 10.0) ?(gamma = 1.0) baseline comparison =
  if gamma >= limit then gamma
  else if hpt_uni ?alpha:r ~comparison:(Array.map (fun x -> x /. gamma) comparison) ~baseline then
    (* [a] significantly outperforms [b] [gamma] times *)
    speedup ?r ~limit ~gamma:(gamma +. 0.01) baseline comparison
  else
    (* We cannot prove that [a] outperforms [b] [gamma] times at [r] confidence level.
     * (Although this might just mean that the performance is identical). *)
    gamma

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
