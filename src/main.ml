open Core.Std
open Fn
open Printf
open Postgresql
open Utils

type build = {
  branch : string;
  build_no : int;
  tag : string;
}

let string_of_build {branch; build_no; tag} =
  sprintf "BUILD (%s, %d, %s)" branch build_no tag

type place =
  | Default
  | ReportCreate of (string * string) list
  | ReportGenerator
  | Reports
  | Report of int
  | Som of int * int list
  | AsyncSom of int * (string * string) list
  | CreateTiny of string
  | RedirectTiny of int

let string_of_int_list is =
  concat (List.map ~f:string_of_int is)

let string_of_som_place name id cids =
  (sprintf "%s: %d" name id) ^ (if cids = [] then "" else
   sprintf " (Config IDs: %s)" (string_of_int_list cids))

let string_of_place = function
  | Default -> "Default"
  | ReportCreate params -> "ReportCreate"
  | ReportGenerator -> "ReportGenerator"
  | Reports -> "Reports"
  | Report id -> sprintf "Report %d" id
  | Som (id, cids) -> string_of_som_place "Som" id cids
  | AsyncSom (id, params) -> string_of_som_place "AsyncSom" id []
  | CreateTiny url -> sprintf "CreateTiny %s" url
  | RedirectTiny id -> sprintf "RedirectTiny %d" id

let path =
  let exe = Sys.argv.(0) in
  String.sub exe ~pos:0 ~len:((String.rindex_exn exe '/') + 1)

let get_request () = Sys.getenv_exn "QUERY_STRING"

let print_header () =
  printf "Content-type: text/html\n\n";
  cat (path ^ "header.html")

let print_footer () = cat (path ^ "footer.html")

let pairs_of_request req =
  let parts = String.split req ~on:'&' in
  let opt_split part =
    Option.value ~default:(part, "") (String.lsplit2 part ~on:'=') in
  List.map ~f:opt_split parts

let string_of_pairs pairs =
  concat ~sep:"\n" (List.map ~f:(fun (k, v) -> k ^ " => " ^ v) pairs)

let values_for_key pairs key =
  List.fold pairs ~init:[]
    ~f:(fun acc (k, v) -> if k = key then v::acc else acc)

let get_first_val params k d =
  Option.value ~default:d (List.hd (values_for_key params k))

let place_of_request req =
  let open List.Assoc in
  let pairs = pairs_of_request req in
  match find pairs "report_generator" with Some _ -> ReportGenerator | None ->
  match find pairs "report_create" with Some _ -> ReportCreate pairs | None ->
  match find pairs "reports" with Some _ -> Reports | None ->
  match find pairs "report" with Some id -> Report (int_of_string id) | None ->
  match find pairs "som" with
  | None ->
    begin match find pairs "t" with
    | Some id -> RedirectTiny (int_of_string id)
    | None ->
      begin match find pairs "action" with
      | Some "CreateTiny" -> CreateTiny (find_exn pairs "url")
      | _ -> Default
      end
    end
  | Some id_s ->
      let id = int_of_string id_s in
      let cids_s = values_for_key pairs "config_id" in
      let cids = List.map ~f:int_of_string cids_s in
      let cids_sorted = List.sort ~cmp:compare cids in
      match find pairs "async" with
      | Some "true" -> AsyncSom (id, pairs)
      | _ -> Som (id, cids_sorted)

let get_place () = place_of_request (get_request ())

let som_config_tbl_exists conn som_id =
  let som_config_tbl = sprintf "som_config_%d" som_id in
  let query = "SELECT * FROM pg_tables WHERE schemaname='public' AND " ^
    (sprintf "tablename='%s'" som_config_tbl) in
  som_config_tbl, (exec_query_exn conn query)#ntuples = 1

let report_generator_handler ~conn =
  print_header ();
  printf "<h2>Report Generator</h2>\n";
  printf "<form action='/' method='get'>\n";
  printf "<input type='hidden' name='report_create' />\n";
  (* Show input boxes for entering basic information. *)
  printf "<hr /><h3>Basic information</h3>\n";
  printf "Report description: <input type='text' name='desc' /><br />\n";
  (* Display standard and all builds in dropboxes. *)
  printf "<hr /><h3>Builds to compare</h3>\n";
  let query = "SELECT build_name, build_number FROM standard_builds " ^
    "ORDER BY build_name" in
  let result = exec_query_exn conn query in
  let standard_builds =
    List.combine_exn (get_col result 0) (get_col result 1) in
  let query =
    "SELECT DISTINCT build_number FROM builds ORDER BY build_number" in
  let all_builds = get_first_col (exec_query_exn conn query) in
  let print_build_options prefix desc =
    printf "<b>%s</b>:<br />\n" desc;
    printf "<table border='1'><tr><th>Standard</th><th>All</th></tr><tr>";
    print_select ~td:true ~selected:["NONE"]
      ~attrs:[("name", prefix ^ "_standard_builds"); ("multiple", "multiple");
              ("size", "3"); ("class", "multiselect")]
      (("NONE", "NONE")::("ALL", "ALL")::standard_builds);
    print_select_list ~td:true ~selected:["NONE"]
      ~attrs:[("name", prefix ^ "_all_builds"); ("multiple", "multiple");
              ("size", "3")]
      ("NONE"::all_builds);
    printf "</tr></table><br />\n";
  in
  print_build_options "primary" "Primary builds (data must be present)";
  print_build_options "secondary"
    "Secondary builds (for comparing against; no data required)";
  (* Show all test cases and their soms. *)
  printf "<hr /><h3>Test cases and their SOMs</h3>\n";
  let query = "SELECT tc_fqn, description FROM test_cases ORDER BY tc_fqn" in
  let test_cases = exec_query_exn conn query in
  let process_tc tc_fqn tc_desc =
    printf "<h4>%s (<i>%s</i>)</h4>\n" tc_fqn tc_desc;
    let tc_tbl = sprintf "tc_config_%s" tc_fqn in
    print_options_for_fields conn tc_tbl tc_fqn;
    let query = "SELECT som_id, som_name FROM soms " ^
      (sprintf "WHERE tc_fqn='%s'" tc_fqn) in
    let soms = exec_query_exn conn query in
    let process_som som_id som_name =
      printf "<input name='include_%s' type='checkbox' />" som_id;
      printf "%s (<i>%s</i>)<br />\n" som_id som_name;
      match som_config_tbl_exists conn (int_of_string som_id) with
      | som_config_tbl, true ->
          print_options_for_fields conn som_config_tbl som_id
      | _ -> ()
    in
    Array.iter ~f:(fun r -> process_som r.(0) r.(1)) soms#get_all
  in
  Array.iter ~f:(fun r -> process_tc r.(0) r.(1)) test_cases#get_all;
  printf "<hr /><input type='submit' value='Create Report' />\n";
  printf "</form>\n";
  print_footer ()

let get_builds_from_params conn params key tbl =
  let build_vals = List.filter ~f:((<>) "NONE") (values_for_key params key) in
  let builds_str =
    if List.mem "ALL" build_vals
    then get_first_col (exec_query_exn conn ("SELECT build_number FROM " ^ tbl))
    else build_vals
  in List.map ~f:int_of_string builds_str

let get_build_group conn params group =
  let std_builds = get_builds_from_params conn params
    (group ^ "_standard_builds") "standard_builds" in
  let all_builds = get_builds_from_params conn params
    (group ^ "_all_builds") "builds" in
  List.sort ~cmp:compare (std_builds @ all_builds)

let report_create_handler ~conn params =
  print_header ();
  (* Obtain metadata. *)
  let desc = List.hd_exn (values_for_key params "desc") in
  (* let query = sprintf "INSERT INTO reports (report_desc) VALUES ('%s')" desc in *)
  (* debug query; *)
  let tuples = [("report_desc", desc, Non_numeric)] in
  let report_id =
    int_of_string (insert_and_get_first_col conn "reports" tuples) in
  (* Gather and record builds. *)
  let primary_builds = get_build_group conn params "primary" in
  let secondary_builds = get_build_group conn params "secondary" in
  (* XXX note that build_tag is assumed to be empty! *)
  let insert_build primary build_number =
    let query = "SELECT build_id FROM builds " ^
      (sprintf "WHERE build_number=%d AND build_tag=''" build_number) in
    debug query;
    match get_first_entry (exec_query_exn conn query) with
    | None ->
      if primary then
        failwith (sprintf "Cannot find build with build_number %d." build_number)
    | Some build_id ->
      let query = "INSERT INTO report_builds (report_id, build_id, \"primary\") " ^
        (sprintf "VALUES (%d, %s, %B)" report_id build_id primary) in
      debug query;
      exec_sql_exn conn query
  in
  List.iter ~f:(insert_build true) primary_builds;
  List.iter ~f:(insert_build false) secondary_builds;
  (* Find included SOMs. *)
  let param_keys = fst (List.split params) in
  let som_ids_str =
    List.filter_map ~f:(String.chop_prefix ~prefix:"include_") param_keys in
  let som_ids = List.map ~f:int_of_string som_ids_str in
  let process_som som_id =
    let tc_fqn =
      let q = sprintf "SELECT tc_fqn FROM soms WHERE som_id=%d" som_id in
      get_first_entry_exn (exec_query_exn conn q)
    in
    let prefix = tc_fqn ^ "_" in
    let tc_config_tbl = "tc_config_" ^ tc_fqn in
    let col_fqns = get_column_fqns conn tc_config_tbl in
    let col_types = get_column_types conn tc_config_tbl in
    let filter = extract_filter col_fqns col_types params prefix in
    let query = sprintf "SELECT tc_config_id FROM %s WHERE %s"
      tc_config_tbl filter in
    let tc_config_ids_str = get_first_col (exec_query_exn conn query) in
    let tc_config_ids = List.map ~f:int_of_string tc_config_ids_str in
    match som_config_tbl_exists conn som_id with
    | som_config_tbl, true ->
        let prefix = sprintf "%d_" som_id in
        let col_fqns = get_column_fqns conn som_config_tbl in
        let col_types = get_column_types conn som_config_tbl in
        let filter = extract_filter col_fqns col_types params prefix in
        let query = sprintf "SELECT som_config_id FROM %s WHERE %s"
          som_config_tbl filter in
        let som_config_ids_str = get_first_col (exec_query_exn conn query) in
        let som_config_ids = List.map ~f:int_of_string som_config_ids_str in
        let insert_report_config tc_config_id som_config_id =
          let query =
            "INSERT INTO report_configs " ^
            "(report_id, som_id, tc_config_id, som_config_id) VALUES " ^
            (sprintf "(%d, %d, " report_id som_id) ^
            (sprintf "%d, %d)" tc_config_id som_config_id) in
          debug query;
          exec_sql_exn conn query
        in
        List.iter tc_config_ids
          ~f:(fun tci -> List.iter som_config_ids ~f:(insert_report_config tci))
    | _ ->
        let insert_report_config tc_config_id =
          let query =
            "INSERT INTO report_configs (report_id, som_id, tc_config_id) " ^
            (sprintf "VALUES (%d, %d, %d)" report_id som_id tc_config_id) in
          debug query;
          exec_sql_exn conn query
        in
        List.iter ~f:insert_report_config tc_config_ids
  in
  List.iter ~f:process_som som_ids;
  (* Print all GET parameters. *)
  List.iter ~f:(fun (k, v) -> printf "%s ===> %s<br />\n" k v) params;
  print_footer ()

let reports_handler ~conn =
  print_header ();
  let query = "SELECT * FROM reports" in
  let result = exec_query_exn conn query in
  let print_col row_i col_i tag data =
    if row_i >= 0 && col_i = 0
    then printf "<%s><a href='/?report=%s'>%s</a></%s>" tag data data tag
    else print_col_default row_i col_i tag data
  in
  print_table_custom_row (print_row_custom ~print_col) result;
  print_footer ()

let report_handler ~conn report_id =
  print_header ();
  printf "<h2>Report ID</h2>\n%d\n" report_id;
  let query =
    sprintf "SELECT report_desc FROM reports WHERE report_id=%d" report_id in
  let desc = get_first_entry_exn (exec_query_exn conn query) in
  printf "<h2>Description</h2>\n%s\n" desc;
  let query =
    "SELECT b.branch, b.build_number, b.build_tag, rb.primary " ^
    "FROM builds AS b, report_builds AS rb " ^
    "WHERE b.build_id = rb.build_id AND " ^
    (sprintf "rb.report_id = %d" report_id) in
  printf "<h2>Builds</h2>\n";
  print_table (exec_query_exn conn query);
  let query =
    "SELECT som_id, tc_config_id, som_config_id FROM report_configs " ^
    (sprintf "WHERE report_id = %d" report_id) in
  printf "<h2>SOMs (and their configurations)</h2>\n";
  let result = exec_query_exn conn query in
  (* print_table result; *)
  let process_config_tuple report_part tuple =
    let som_id : int = int_of_string tuple.(0) in
    let tc_config_id : int = int_of_string tuple.(1) in
    let som_config_id_opt : int option =
      if tuple.(2) = "" then None else Some (int_of_string tuple.(2)) in
    let som_config_id_opt_str =
      Option.value_map som_config_id_opt ~f:string_of_int ~default:"none" in
    let query = "SELECT som_name, tc_fqn FROM soms " ^
      (sprintf "WHERE som_id = %d" som_id) in
    let som_info = (exec_query_exn conn query)#get_tuple 0 in
    let som_name = som_info.(0) in
    let tc_fqn = som_info.(1) in
    let query = "SELECT description FROM test_cases " ^
      (sprintf "WHERE tc_fqn = '%s'" tc_fqn) in
    let tc_desc = get_first_entry_exn (exec_query_exn conn query) in
    let query =
      (sprintf "SELECT * FROM tc_config_%s " tc_fqn) ^
      (sprintf "WHERE tc_config_id = %d" tc_config_id) in
    let tc_config_info = exec_query_exn conn query in
    let som_config_info_opt =
      match som_config_id_opt with None -> None | Some som_config_id ->
      let query =
        (sprintf "SELECT * FROM som_config_%d " som_id) ^
        (sprintf "WHERE som_config_id = %d" som_config_id) in
      Some (exec_query_exn conn query)
    in
    let print_config_info_part config_info desc col =
      let fname = config_info#fname col in
      let value = config_info#getvalue 0 col in
      printf "%s config \"%s\": %s<br />\n" desc fname value
    in
    printf "<h3>Part %d</h3>\n" (report_part + 1);
    printf "SOM id: %d<br />\n" som_id;
    printf "SOM name: %s<br />\n" som_name;
    printf "TC fqn: %s<br />\n" tc_fqn;
    printf "TC description: %s<br />\n" tc_desc;
    printf "TC configuration id: %d<br />\n" tc_config_id;
    List.iter ~f:(print_config_info_part tc_config_info "TC")
      (List.range 1 tc_config_info#nfields);
    printf "SOM configuration id: %s<br />\n" som_config_id_opt_str;
    begin
      match som_config_info_opt with None -> () | Some som_config_info ->
      List.iter ~f:(print_config_info_part som_config_info "SOM")
        (List.range 1 som_config_info#nfields);
    end
  in
  Array.iteri ~f:process_config_tuple result#get_all;
  printf "<script src='rage.js'></script>";
  print_footer ()

let default_handler ~place ~conn =
  print_header ();
  let query = "SELECT som_id, som_name FROM soms " ^
              "ORDER BY som_id" in
  let result = exec_query_exn conn query in
  let print_row row_i tag row =
    match row_i with -1 -> print_row_header row | _ ->
    print_string "   <tr>";
    let som_id = List.nth_exn row 0 in
    let name = List.nth_exn row 1 in
    printf "<%s>%s</%s>" tag som_id tag;
    printf "<%s><a href='?som=%s'>%s</a></%s>" tag som_id name tag;
    print_string "   </tr>" in
  print_table_custom_row print_row result;
  print_footer ()

let get_tc_config_tbl_name conn som_id =
  let query = "SELECT tc_fqn FROM soms " ^
              "WHERE som_id = " ^ (string_of_int som_id) in
  let result = exec_query_exn conn query in
  let tc_fqn = String.lowercase (result#getvalue 0 0) in
  (tc_fqn, "tc_config_" ^ tc_fqn)

let get_branch_ordering ~conn branches =
  let branches_uniq = List.dedup ~compare branches in
  let query =
    "SELECT branch FROM branch_order WHERE branch IN ('" ^
    (concat ~sep:"','" branches_uniq) ^ "') ORDER BY seq_number" in
  debug query;
  let data = exec_query_exn conn query in
  let process_row i row = (i+1, List.nth_exn row 0) in
  Some (List.mapi data#get_all_lst ~f:process_row)

let print_som_info som_info =
  let i = som_info#get_all.(0) in
  let name = sprintf "<span class='som_name'>%s</span>" i.(1) in
  let prefix = "<div class='som'>SOM:" in
  let suffix = "</div><br />\n" in
  printf "%s %s (id: %s, tc: %s) %s" prefix name i.(0) i.(2) suffix

let get_xy_choices configs som_configs_opt machines =
  let som_configs_lst = match som_configs_opt with
    | None -> []
    | Some som_configs -> List.tl_exn som_configs#get_fnames_lst
  in
  "branch" :: "build_number" :: "build_tag" ::
    machines#get_fnames_lst @ configs#get_fnames_lst @ som_configs_lst

let print_axis_choice label id choices =
  printf "<div id='%s' style='display: inline'>\n" id;
  print_select_list ~label ~attrs:[("name", id)] choices;
  printf "</div>\n"

let print_x_axis_choice configs som_configs_opt machines =
  print_axis_choice "X axis" "xaxis"
    (get_xy_choices configs som_configs_opt machines)

let print_y_axis_choice configs som_configs_opt machines =
  print_axis_choice "Y axis" "yaxis"
    ("result" :: (get_xy_choices configs som_configs_opt machines))

let filter_prefix = "f_"
let values_prefix = "v_"
let show_for_value = "0"
let filter_by_value = "1"

let print_filter_table job_ids builds configs som_configs_opt machines =
  (* LABELS *)
  let som_config_labels =
    match som_configs_opt with None -> [] | Some som_configs ->
    List.tl_exn (som_configs#get_fnames_lst) in
  let labels = "job_id" :: "branch" :: "build_number" :: "build_tag" ::
    machines#get_fnames_lst @ configs#get_fnames_lst @ som_config_labels in
  (* OPTIONS *)
  let job_id_lst = get_options_for_field job_ids 0 in
  let branch_lst = get_options_for_field builds 0 in
  let build_no_lst = get_options_for_field builds 1 in
  let tag_lst = get_options_for_field builds 2 in
  let machine_options_lst = List.map (List.range 0 machines#nfields)
    ~f:(fun col -> get_options_for_field machines col) in
  let config_options_lst = List.map (List.range 0 configs#nfields)
    ~f:(fun col -> get_options_for_field configs col) in
  let som_config_options_lst =
    match som_configs_opt with None -> [] | Some som_configs ->
    List.map (List.range 1 som_configs#nfields)
      ~f:(fun col -> get_options_for_field som_configs col) in
  let options_lst = job_id_lst :: branch_lst :: build_no_lst ::
    tag_lst :: machine_options_lst @ config_options_lst @
    som_config_options_lst in
  let print_table_for (label, options) =
    printf "<table border='1' class='filter_table'>\n";
    printf "<tr><th>%s</th></tr>\n" label;
    printf "<tr>\n";
    print_select ~td:true
      ~attrs:[("name", filter_prefix ^ label); ("class", "filterselect")]
      [("SHOW FOR", show_for_value); ("SPLIT BY", filter_by_value)];
    printf "</tr><tr>\n";
    print_select_list ~td:true ~selected:["ALL"]
      ~attrs:[("name", values_prefix ^ label); ("multiple", "multiple");
              ("size", "3"); ("class", "multiselect")]
      ("ALL"::options);
    printf "</tr></table>\n"
  in
  List.iter ~f:print_table_for (List.combine_exn labels options_lst)

let show_configurations ~conn som_id tc_config_tbl =
  let query =
    sprintf "SELECT * FROM soms WHERE som_id=%d" som_id in
  let som_info = exec_query_exn conn query in
  let query = "SELECT * FROM " ^ tc_config_tbl in
  let configs = exec_query_exn conn query in
  let query = "SELECT DISTINCT job_id FROM measurements WHERE " ^
    (sprintf "som_id=%d" som_id) in
  let job_ids = exec_query_exn conn query in
  let query =
    "SELECT DISTINCT branch, build_number, build_tag " ^
    "FROM builds AS b, jobs AS j, measurements AS m " ^
    "WHERE m.job_id=j.job_id AND j.build_id=b.build_id " ^
    (sprintf "AND m.som_id=%d" som_id) in
  let builds = exec_query_exn conn query in
  let som_config_tbl, som_tbl_exists = som_config_tbl_exists conn som_id in
  let som_configs_opt =
    if not som_tbl_exists then None else
    Some (exec_query_exn conn (sprintf "SELECT * FROM %s" som_config_tbl)) in
  let query =
    "SELECT DISTINCT machine_name, machine_type, cpu_model, number_of_cpus " ^
    "FROM machines AS mn, tc_machines AS tm, measurements AS mr " ^
    "WHERE mn.machine_id=tm.machine_id AND tm.job_id=mr.job_id " ^
    (sprintf "AND som_id=%d" som_id) in
  let machines = exec_query_exn conn query in
  print_som_info som_info;
  print_select_list ~label:"View" ~attrs:[("id", "view")] ["Graph"; "Table"];
  printf "<form name='optionsForm'>\n";
  print_x_axis_choice configs som_configs_opt machines;
  print_y_axis_choice configs som_configs_opt machines;
  let checkbox name caption =
    printf "<div id='%s' style='display: inline'>\n" name;
    printf "<input type='checkbox' name='%s' />%s\n" name caption;
    printf "</div>\n" in
  checkbox "show_avgs" "Show averages";
  checkbox "x_from_zero" "Force X from 0";
  checkbox "y_from_zero" "Force Y from 0";
  checkbox "show_all_meta" "Show all meta-data";
  checkbox "yaxis_log" "Log scale Y";
  printf "<br />\n";
  print_filter_table job_ids builds configs som_configs_opt machines;
  printf "</form>\n";
  let submit_prefix = "<input type='submit' id=" in
  printf "%s'reset_config' value='Reset Configuration' />" submit_prefix;
  printf "%s'get_img' value='Get Image' />" submit_prefix;
  printf "%s'get_tinyurl' value='Get Tiny URL' />" submit_prefix;
  printf "<a id='tinyurl' style='display: none' title='Tiny URL'></a>";
  printf "<br /><img id='progress_img' src='progress.gif' />\n";
  printf "<div id='graph' style='width: 1000px; height: 600px'></div>";
  printf "<div id='table'></div>";
  printf "<script src='rage.js'></script>"

let som_handler ~place ~conn som_id config_ids =
  print_header ();
  let tc_fqn, tc_config_tbl = get_tc_config_tbl_name conn som_id in
  show_configurations ~conn som_id tc_config_tbl;
  print_footer ()

(** Pre-generated regexp for use within asyncsom_handler. *)
let quote_re = Str.regexp "\""

let get_generic_string_mapping rows col col_name col_types =
  match String.Table.find col_types col_name with None -> None | Some ty ->
  match ty = "character varying" with false -> None | true ->
  let uniques = List.dedup (extract_column rows col) in
  let sorted = List.sort ~cmp:compare uniques in
  Some (List.mapi sorted ~f:(fun i x -> (i+1, x)))

let strings_to_numbers ~conn rows col col_name col_types label =
  let mapping_opt =
    (*if col_name = "branch"
    then get_branch_ordering ~conn (extract_column rows col)
    else*) get_generic_string_mapping rows col col_name col_types
  in
  match mapping_opt with None -> () | Some mapping ->
  let process_entry (i, a) = sprintf "\"%d\":\"%s\"" i a in
  let mapping_str = concat (List.map mapping ~f:process_entry) in
  printf "\"%s\":{%s}," label mapping_str;
  let i_to_string_map = List.Assoc.inverse mapping in
  let i_from_string row =
    match List.Assoc.find i_to_string_map row.(col) with
    | None -> failwith ("NOT IN TRANSLATION MAP: " ^ row.(col) ^ "\n")
    | Some i -> row.(col) <- string_of_int i
  in Array.iter rows ~f:i_from_string

let select_params params ?(value=None) prefix =
  List.filter_map params ~f:(fun (k, v) ->
    if String.is_prefix k prefix && (Option.is_none value || Some v = value)
    then String.chop_prefix k ~prefix else None
  )
let get_filter_params params = select_params params filter_prefix
let get_values_params params = select_params params values_prefix
let get_relevant_params params =
  get_filter_params params @ get_values_params params

let asyncsom_handler ~conn som_id params =
  printf "Content-type: application/json\n\n";
  let tc_fqn, tc_config_tbl = get_tc_config_tbl_name conn som_id in
  let som_config_tbl, som_tbl_exists = som_config_tbl_exists conn som_id in
  (* determine filter columns and their types *)
  let tbls = ["measurements"; "jobs"; "builds"; "tc_machines"; "machines";
    tc_config_tbl] @
    (if som_tbl_exists then [som_config_tbl] else []) in
  let col_fqns = get_column_fqns_many conn tbls in
  let col_types = get_column_types_many conn tbls in
  let xaxis = get_first_val params "xaxis" "branch" in
  let yaxis = get_first_val params "yaxis" "result" in
  let keys = match get_first_val params "show_all_meta" "off" with
    | "on" ->
      xaxis :: yaxis :: String.Table.keys col_fqns |! List.stable_dedup
    | _ ->
      xaxis :: yaxis :: get_relevant_params params |! List.stable_dedup
  in
  let fqns = List.map keys ~f:(String.Table.find_exn col_fqns) in
  let filter = extract_filter col_fqns col_types params values_prefix in
  (* obtain data from database *)
  let query =
    (sprintf "SELECT %s " (String.concat ~sep:", " fqns)) ^
    (sprintf "FROM %s " (String.concat ~sep:", " tbls)) ^
    (sprintf "WHERE measurements.tc_config_id=%s.tc_config_id "
             tc_config_tbl) ^
    (sprintf "AND measurements.som_id=%d " som_id) ^
    "AND measurements.job_id=jobs.job_id " ^
    "AND jobs.build_id=builds.build_id " ^
    "AND tc_machines.job_id=jobs.job_id " ^
    (sprintf "AND tc_machines.tc_fqn='%s' " tc_fqn) ^
    "AND tc_machines.tc_config_id=measurements.tc_config_id " ^
    "AND tc_machines.machine_id=machines.machine_id" ^
    (if som_tbl_exists
     then sprintf " AND measurements.som_config_id=%s.som_config_id"
          som_config_tbl else "") ^
    (if not (String.is_empty filter) then sprintf " AND %s" filter else "") in
  debug query;
  let data = exec_query_exn conn query in
  let rows = data#get_all in
  (* filter data into groups based on "SPLIT BY"-s *)
  let split_bys =
    select_params params filter_prefix ~value:(Some filter_by_value) in
  let split_by_is = List.map split_bys ~f:(index keys) in
  let all_series = ListKey.Table.create () in
  let get_row_key row is = List.map is ~f:(Array.get row) in
  let add_to_series row series_opt =
    match series_opt with
    | None -> Some [row]
    | Some rows -> Some (row::rows)
  in
  let update_all_series row =
    let row_key = get_row_key row split_by_is in
    ListKey.Table.change all_series row_key (add_to_series row)
  in
  Array.iter rows ~f:update_all_series;
  (* output axis labels and a "series" for each data group *)
  printf "{";
  printf "\"xaxis\":\"%s\"," xaxis;
  printf "\"yaxis\":\"%s\"," yaxis;
  strings_to_numbers ~conn rows 0 xaxis col_types "x_labels";
  strings_to_numbers ~conn rows 1 yaxis col_types "y_labels";
  let num_other_keys = List.length keys - 2 in
  let convert_row row =
    let other_vals = Array.sub row ~pos:2 ~len:num_other_keys in
    let process_val i v =
      let v' = Str.global_replace quote_re "\\\"" v in
      "\"" ^ (List.nth_exn keys (i+2)) ^ "\":\"" ^ v' ^ "\"" in
    let prop_array = Array.mapi other_vals ~f:process_val in
    let props = concat_array prop_array in
    "[" ^ row.(0) ^ "," ^ row.(1) ^ ",{" ^ props ^ "}]"
  in
  printf "\"series\":[";
  let process_series i (row_key, rows) =
    let pairs = List.map2_exn split_bys row_key ~f:(fun k v -> k ^ "=" ^ v) in
    let label = concat pairs in
    let data_lst = List.map rows ~f:convert_row in
    let data_str = concat data_lst in
    sprintf "{\"label\":\"%s\",\"color\":%d,\"data\":[%s]}" label i data_str
  in
  let json_list =
    List.mapi (ListKey.Table.to_alist all_series) ~f:process_series in
  print_string (concat json_list);
  printf "]}"

let createtiny_handler ~conn url =
  printf "Content-type: application/json\n\n";
  let tbl = "tiny_urls" in
  let select = sprintf "SELECT key FROM %s WHERE url='%s'" tbl url in
  let r = exec_query_exn conn select in
  match r#ntuples with
  | 1 -> printf "{\"id\":%s}" (r#getvalue 0 0)
  | _ ->
    let insert = sprintf "INSERT INTO %s (url) VALUES ('%s')" tbl url in
    ignore (exec_query_exn conn insert);
    let r = exec_query_exn conn select in
    printf "{\"id\":%s}" (r#getvalue 0 0)

let print_404 () =
  printf "Status: 404 Not Found\n";
  printf "Content-Type: text/html\n\n";
  printf "<h1>404 --- this is not the page you are looking for ...</h1>"

let javascript_redirect url =
  printf "Content-type: text/html\n\n";
  printf "<html><head>\n";
  printf "<script language='javascript' type='text/javascript'>\n";
  printf "window.location = decodeURIComponent('%s');\n" url;
  printf "</script>\n</head></html>\n"

let redirecttiny_handler ~conn id =
  let q = sprintf "SELECT url FROM tiny_urls WHERE key=%d" id in
  let r = exec_query_exn conn q in
  match r#ntuples with
  | 1 -> javascript_redirect (r#getvalue 0 0)
  | _ -> print_404 ()

let handle_request () =
  let place = get_place () in
  let conn = new connection ~conninfo:Sys.argv.(1) () in
  begin match place with
    | Som (id, cids) -> som_handler ~place ~conn id cids
    | AsyncSom (id, params) -> asyncsom_handler ~conn id params
    | CreateTiny url -> createtiny_handler ~conn url
    | RedirectTiny id -> redirecttiny_handler ~conn id
    | ReportCreate params -> report_create_handler ~conn params
    | ReportGenerator -> report_generator_handler ~conn
    | Reports -> reports_handler ~conn
    | Report id -> report_handler ~conn id
    | Default -> default_handler ~place ~conn
  end;
  conn#finish

let _ =
  try handle_request ()
  with Failure msg -> Printexc.print_backtrace stderr; printf "<b>%s</b>" msg
