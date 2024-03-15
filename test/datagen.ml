(** Generate sequences of values with a known distribution for testing purposes.
    Note that these aren't all random numbers, but sequences that match the given distribution's parameters
 *)

module Uniform = struct
    let rand n =
        Array.init n @@ fun _ -> Random.float 1.0

    let linspace n =
        let delta = 1. /. float_of_int (n + 1) in
        Array.init n @@ fun i ->
        delta *. (float_of_int @@ i + 1)

    let low_discrepancy =
        (* fractional part of golden ratio *)
        let c = (sqrt 5. -. 1.) /. 2. in
        fun n -> 
            Array.init n @@ fun i ->
            let frac, _ = Float.modf @@ (i + 1 |> float_of_int) *. c in
            frac

    (* also tried Zar, but when transforming back to float it has errors in the computed mean *)
end

module Normal = struct
    let rand1 ~mu ~sigma n =
        Array.init n @@ fun _ ->
        (* this currently uses Box-Muller transform *)
        Owl_base.Stats.gaussian_rvs ~mu ~sigma

    let rand2 ~mu ~sigma n =
        Array.init n @@ fun _ ->
        (* this currently uses the Ziggurat algorithm. The algorithm has some known flaws. *)
        Owl.Stats.gaussian_rvs ~mu ~sigma

    open Owl
    let fixed ~mu ~sigma n =
        Uniform.linspace n |> Array.map (Stats.gaussian_isf ~mu ~sigma)

    let low_discrepancy ~mu ~sigma n =
        Uniform.low_discrepancy n |> Array.map (Stats.gaussian_isf ~mu ~sigma)
end
