(* Compute a SoM's metric for a particular job
 * This will be a list of the measurements. *)

module type SOM = sig
	val compute_metrics : int -> float list
end

let resultsdir = "/usr/groups/perfeng/results"

module SOMmigratedowntime = struct
	let tcdir = "reqmigratedowntime"
	let logfile = "downtime.log"

	let compute_metrics job =
		let filename = Printf.sprintf "%s/%s/runs/job-%d/%s" resultsdir tcdir job logfile in
		let lines = List.filter Helpers.nothash (Helpers.read_lines filename) in
		List.map (fun line ->
			let fields = Helpers.split line '	' in
			float_of_string (List.nth fields 1)
		) lines
end

module SOMmigrateduration = struct
	let tcdir = "reqmigratedowntime"
	let logfile = "downtime.log"

	let compute_metrics job =
		let filename = Printf.sprintf "%s/%s/runs/job-%d/%s" resultsdir tcdir job logfile in
		let lines = List.filter Helpers.nothash (Helpers.read_lines filename) in
		List.map (fun line ->
			let fields = Helpers.split line '	' in
			float_of_string (List.nth fields 2)
		) lines
end

let process somid jobid f_computemetrics =
	let metrics = f_computemetrics jobid in
	Helpers.write_results_to_db somid jobid metrics

let _ =
	process 2 226858 SOMmigratedowntime.compute_metrics;
	process 2 226860 SOMmigratedowntime.compute_metrics;
	process 12 226858 SOMmigrateduration.compute_metrics;
	process 12 226860 SOMmigrateduration.compute_metrics;
