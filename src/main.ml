open Core.Std
open Printf
open Postgresql
open Utils
open Flot

(*
let get_request_uri () = Sys.getenv "REQUEST_URI"

let request () =
  let request_uri = get_request_uri () in
  let len = String.length request_uri in
  String.sub request_uri 2 (len - 2)

let cond () =
  String.concat " AND " (Str.split (Str.regexp "&") (request ()))
*)

let extract_data x_axis_reverse_labels db_result =
  let extract_entry row =
    let x_label = List.nth_exn row 0 in
    let x_opt = Hashtbl.find x_axis_reverse_labels x_label in
    let x = Option.value x_opt ~default:0 in
    let y = int_of_string (List.nth_exn row 1) in
      (x, y)
  in List.map db_result#get_all_lst (fun row -> extract_entry row)

let exec_query (conn : connection) (query : string) : result option =
  let result = conn#exec query in
  match result#status with
    | Command_ok | Tuples_ok -> Some result
    | _ -> print_endline conn#error_message; None

let _ =
  let exe = Sys.argv.(0) in
  let path = String.sub exe ~pos:0 ~len:((String.rindex_exn exe '/') + 1) in
  printf "Content-type: text/html\n\n";
  let query = "SELECT tc_network.build, results.result " ^
    "FROM tc_network INNER JOIN results ON " ^
    "tc_network.job_id = results.job_id WHERE tc_network.build LIKE '%-ga' " ^
    "ORDER BY tc_network.build, results.job_id, results.result_id" in
  let conn = new connection ~conninfo:Sys.argv.(1) () in
  begin
    match exec_query conn query with None -> () | Some result ->
    cat (path ^ "header.html");
    printf "  <center><h2>dom0 network throughput</h2></center>\n";
    let settings =
      [X_axis [Min 0; Max 5; TickFormatter "tf"]; Y_axis [Min 0]] in
    let x_axis_labels =
      natural_map_from_list ["mnr-ga"; "cowley-ga"; "oxford-ga"; "boston-ga"] in
    let x_axis_reverse_labels = reverse_natural_map x_axis_labels in
    let data = extract_data x_axis_reverse_labels result in
    print_string (plot ~settings ~labels:x_axis_labels data);
    cat (path ^ "footer.html")
  end;
  conn#finish
