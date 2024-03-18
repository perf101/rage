open Owl
module Uniform = struct
  (** a random number generator in the range [[0, 1)] *)
  let rng _ =
    (* Printf.sprintf "%h" (Float.pred 1.) *)
    Random5.float 0x1.fffffffffffffp-1
  
  let random n = Array.init n rng

  (** [equidistributed n] generates [n] numbers that are equidistributed on [[0, 1)] *)
  let equidistributed n =
    (* uses the Van-der-Corput sequence: 1/2,1/4,3/4,1/8,5/8,... *)
    let rec fill a i nom denom =
      if i < n then begin
        a.(i) <- nom /. denom;
        let nom = nom +. 2.
        and i = i + 1 in
        if nom > denom then
          fill a i 1. (denom *. 2.)
        else
          fill a i nom denom
      end        
    in
    let a = Array.create_float n in
    fill a 0 1. 2.;
    a
      
  (* more than 10^4, and near a power of 2, so that [mean] of [equidistributed n] is exact. *)
  let fixed =
    let a = equidistributed (1 lsl 14 - 1) in
    (* to limit the impact of bugs, we always return a copy *)
    Array.copy a
end

module Normal = struct
  (** [inverse_cdf ~mu ~sigma x] is the inverse Cumulative Distribution Function
    for the Normal distribution with mean [mu] and standard deviation [sigma]
   *)
  let inverse_cdf ~mu ~sigma x = Stats.gaussian_isf ~mu ~sigma (1. -. x)

  (** [of_uniform ~mu ~sigma a] creates an array of normally distributed values with mean [mu] and standard deviation [sigma],
      given an array of uniform values from the range [(0,1)]
   *)
  let of_uniform ~mu ~sigma a =
    a |> Array.map @@ fun x ->
    (* apply the inverse CDF, ISF = SF^-1, SF = 1 - CDF *)
    inverse_cdf ~mu ~sigma x
  
  let random ~mu ~sigma n =
    Uniform.random n |> of_uniform ~mu ~sigma

  let fixed_accurate =
    (* sigma is power of 2 *)
    Uniform.fixed |> of_uniform ~mu:1. ~sigma:0x1p-23

  let random_accurate () =
    Uniform.random (Array.length Uniform.fixed)
    |> of_uniform ~mu:1. ~sigma:0x1p-23
    (*Array.init (1 lsl 16 - 1) @@ (fun _ -> 1. +. 0x1p-23 *. Owl_stats_prng.rand_gaussian ())*)

  let fixed2 =
    Uniform.fixed |> of_uniform ~mu:2. ~sigma:0.2

  let random2 () =
    Uniform.random (Array.length Uniform.fixed)
    |> of_uniform ~mu:2. ~sigma:0.2
  
end

(* TODO: some multimodal (sum of 2 normals?), e.g. 2 machines *)
