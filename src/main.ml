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

type place = Default | Som of int * int list

let string_of_int_list is =
  String.concat ~sep:", " (List.map ~f:string_of_int is)

let string_of_place = function
  | Default -> "Default"
  | Som (id, cids) ->
      (sprintf "Som: %d" id) ^ (if cids = [] then "" else
       sprintf " (Config IDs: %s)" (string_of_int_list cids))

let pairs_of_request req =
  let params = String.drop_prefix req 2 in
  let parts = String.split params ~on:'&' in
  List.map ~f:(fun p -> String.lsplit2_exn p ~on:'=') parts

let values_for_key pairs key =
  List.fold pairs ~init:[]
    ~f:(fun acc (k, v) -> if k = key then v::acc else acc)

let place_of_request req =
  match String.is_prefix req ~prefix:"/?" with
  | false -> Default
  | true ->
    let pairs = pairs_of_request req in
    match values_for_key pairs "som" with
    | [] -> Default
    | id_s::_ ->
        let cids_s = values_for_key pairs "config_id" in
        let cids = List.map ~f:int_of_string cids_s in
        Som (int_of_string id_s, List.sort ~cmp:compare cids)

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
  let print_row row_i tag row =
    match row_i with -1 -> print_row_header row | _ ->
    print_string "   <tr>";
    let som_id = List.nth_exn row 0 in
    let name = List.nth_exn row 1 in
    printf "<%s>%s</%s>" tag som_id tag;
    printf "<%s><a href='?som=%s'>%s</a></%s>" tag som_id name tag;
    print_string "   </tr>" in
  print_table_custom_row print_row result

let extract_data x_axis_reverse_labels db_result =
  let dbfn = db_result#fnumber in
  let config_id_col, build_col, result_col =
    dbfn "config_id", dbfn "build", dbfn "result" in
  let data = Int.Table.create () in
  let update_points p = function
    | None -> Some [p]
    | Some ps -> Some (p::ps)
  in
  let extract_entry row =
    let config_id = int_of_string (List.nth_exn row config_id_col) in
    let x_label = List.nth_exn row build_col in
    let x_opt = Hashtbl.find x_axis_reverse_labels x_label in
    let x = Option.value x_opt ~default:0. in
    let y = float_of_string (List.nth_exn row result_col) in
    Int.Table.change data config_id (update_points (x, y))
  in List.iter db_result#get_all_lst (fun row -> extract_entry row);
  List.mapi (Int.Table.data data)
    ~f:(fun i s ->
      {label = "config_id=" ^ (string_of_int i);
       points = s;
       line_settings = line_settings_default})

let generate_average {label; points; line_settings} =
  let sums = Float.Table.create () in
  let update v count_sum_opt =
    let count, sum = Option.value count_sum_opt ~default:(0, 0.) in
    Some (count + 1, sum +. v) in
  List.iter points
    ~f:(fun (x, y) -> Float.Table.change sums x (update y));
  let avg_map = Float.Table.map sums
    ~f:(fun (c, s) -> s /. (float_of_int c)) in
  let avg_list = Float.Table.to_alist avg_map in
  let avg_list_sorted =
    List.sort avg_list ~cmp:(fun (x1,_) (x2,_) -> compare x1 x2) in
  {label = label ^ ", average";
   points = avg_list_sorted;
   line_settings = {show_lines = Some true}}

let add_averages data =
  let data_avg = List.map data ~f:generate_average in
  List.rev_append data data_avg

let som_handler ~conn som_id config_ids =
  let query = "SELECT tc FROM tbl_som_definitions " ^
              "WHERE som_id = " ^ (string_of_int som_id) in
  match exec_query ~conn query with None -> () | Some result ->
  let tc = String.lowercase (result#getvalue 0 0) in
  let config_tbl = "tbl_config_tc_" ^ tc in
  let jobs_tbl = "tbl_jobs_tc_" ^ tc in
  printf "TC: %s<br />\n" tc;

  if config_ids <> [] then begin
    let fields = "config_id, build, result" in
    let query =
      (sprintf "SELECT %s FROM tbl_measurements " fields)^
      (sprintf "WHERE som_id = %d AND job_id IN" som_id) ^
      (sprintf "(SELECT job_id FROM %s WHERE config_id IN (%s))"
        jobs_tbl (string_of_int_list config_ids)) ^
        (sprintf " ORDER BY %s" fields) in
    match exec_query ~conn query with None -> () | Some result ->
    let builds = Array.map result#get_all ~f:(fun row -> row.(1)) in
    let builds_uniq = List.dedup ~compare (Array.to_list builds) in
    let query =
      "SELECT name FROM tbl_versions WHERE name IN ('" ^
      (String.concat ~sep:"', '" builds_uniq) ^ "') ORDER BY seq_no" in
    match exec_query ~conn query with None -> () | Some v_result ->
    let builds_ord = List.flatten v_result#get_all_lst in
    let x_axis_labels = natural_map_from_list builds_ord in
    let x_axis_reverse_labels = reverse_natural_map x_axis_labels in
    let settings = {settings_default with
      xaxis = {axis_default with
        tickFormatter = Some x_axis_labels; tickSize = Some 1.};
      yaxis = {axis_default with min = Some 0.}} in
    let raw_data = extract_data x_axis_reverse_labels result in
    let data = add_averages raw_data in
    let plot = {dom_id = "graph"; data; settings} in
    print_string (string_of_plot plot)
    (*List.iter builds_ord ~f:(fun b -> printf "%s<br />\n" b);*)
    (*print_table result*)
  end;

  let query = "SELECT * FROM " ^ config_tbl in
  match exec_query ~conn query with None -> () | Some result ->
  let print_col row_i col_i tag data =
    begin match row_i, col_i with
      | -1, 0 ->
        print_col_default row_i col_i tag "Select";
      | _, 0 ->
        let checkbox = sprintf
          "<input type='checkbox' name='config_id' value='%s' />" data in
        print_col_default row_i col_i tag checkbox;
      | _ -> ()
    end;
    print_col_default row_i col_i tag data in
  printf "<form action='/' method='get'>\n";
  printf "<input type='hidden' name='som' value='%d' />" som_id;
  print_table_custom_col print_col result;
  printf "<input type='submit' value='Display' />";
  printf "</form>"

let handle_request () =
  printf "Content-type: text/html\n\n";
  print_header ();
  let place = get_place () in
  printf "Request: %s<br />\n" (get_request ());
  printf "Place: %s<br />\n" (string_of_place place);
  let conn = new connection ~conninfo:Sys.argv.(1) () in
  begin match place with
    | Som (id, cids) -> som_handler ~conn id cids
    | Default -> default_handler ~conn
  end;
  conn#finish;
  print_footer ()

let _ =
  handle_request ()
