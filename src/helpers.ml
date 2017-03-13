(* helper functions *)

let read_lines filename =
	let chan = open_in filename in
	let rec read_lines () =
		try
			let line = input_line chan in
			line :: read_lines ()
		with End_of_file -> []
	in
	read_lines ()

let nothash line = String.sub line 0 1 <> "#"

let split str sep =
	let rec chars str i acc =
		if i = String.length str then [] else begin
			let c = String.get str i in
			if c = sep then
				acc :: (chars str (i+1) "")
			else
				chars str (i+1) (acc ^ (String.make 1 c))
		end
	in chars str 0 ""

let list_iter_with_index f xs =
	let rec iter n = function [] -> ()
	  | (x::xs) ->
	        f n x;
	        iter (n+1) xs
	in iter 0 xs

open Postgresql

let exec_query (conn : connection) (query : string) : result option =
  let result = conn#exec query in
  match result#status with
    | Command_ok | Tuples_ok -> Some result
    | _ -> print_endline conn#error_message; None

let conn = new connection ~conninfo:Sys.argv.(1) ()

let write_results_to_db somid jobid metrics =
	let resultstable = "results" in
	list_iter_with_index (fun i value ->
		let query = Printf.sprintf "INSERT INTO %s VALUES (%d, %d, %d, %f)" resultstable somid jobid i value in
		exec_query conn query
	) metrics
