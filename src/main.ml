open Core.Std
open Printf
open Postgresql
open Utils
open Flot

let path =
  let exe = Sys.argv.(0) in
  String.sub exe ~pos:0 ~len:((String.rindex_exn exe '/') + 1)
let print_header () = cat (path ^ "header.html")
let print_footer () = cat (path ^ "footer.html")

type place = Default | Som of int

let string_of_place = function
  | Default -> "Default"
  | Som id -> "Som: " ^ (string_of_int id)

let place_of_request req =
  if String.is_prefix req ~prefix:"/?som="
  then Som (int_of_string (String.drop_prefix req 6))
  else Default

let get_request () = Sys.getenv_exn "REQUEST_URI"

let get_place () = place_of_request (get_request ())

let exec_query ~(conn : connection) (query : string) : result option =
  let result = conn#exec query in
  match result#status with
    | Command_ok | Tuples_ok -> Some result
    | _ -> print_endline conn#error_message; None

let default_handler ~conn =
  let query = "SELECT som_id, name FROM tbl_som_definitions " ^
              "ORDER BY som_id" in
  match exec_query ~conn query with None -> () | Some result ->
  let print_row tag elems =
    print_string "   <tr>";
    let som_id = List.nth_exn elems 0 in
    let name = List.nth_exn elems 1 in
    printf "<td>%s</td>" som_id;
    printf "<td><a href='?som=%s'>%s</a></td>" som_id name;
    print_string "   </tr>" in
  print_table ~print_row result

let som_handler ~conn som_id =
  let query = "SELECT tc FROM tbl_som_definitions " ^
              "WHERE som_id = " ^ (string_of_int som_id) in
  match exec_query ~conn query with None -> () | Some result ->
  let tc = String.lowercase (result#getvalue 0 0) in
  let config_tbl = "tbl_config_tc_" ^ tc in
  let jobs_tbl = "tbl_jobs_tc_" ^ tc in
  printf "TC: %s<br />\n" tc;
  printf "CONFIG TABLE: %s<br />\n" config_tbl;
  printf "JOBS TABLE: %s<br />\n" jobs_tbl

let handle_request () =
  printf "Content-type: text/html\n\n";
  print_header ();
  let place = get_place () in
  printf "Request: %s<br />\n" (get_request ());
  printf "Place: %s<br />\n" (string_of_place place);
  let conn = new connection ~conninfo:Sys.argv.(1) () in
  begin match place with
    | Som id -> som_handler ~conn id
    | Default -> default_handler ~conn
  end;
  conn#finish;
  print_footer ()

let extract_data x_axis_reverse_labels db_result =
  let extract_entry row =
    let x_label = List.nth_exn row 0 in
    let x_opt = Hashtbl.find x_axis_reverse_labels x_label in
    let x = Option.value x_opt ~default:0 in
    let y = int_of_string (List.nth_exn row 1) in
      (x, y)
  in List.map db_result#get_all_lst (fun row -> extract_entry row)

let _ =
  handle_request ()

(*
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
*)
