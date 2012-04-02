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
  | Report
  | ReportPart of int
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
  | Report -> "Report"
  | ReportPart id -> sprintf "ReportPart %d" id
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
  match find pairs "reports" with Some _ -> Report | None ->
  match find pairs "report_part" with
  | Some id -> ReportPart (int_of_string id)
  | None ->
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

let report_handler ~place ~conn =
  print_header ();
  let query = "SELECT * FROM reports" in
  let result = exec_query_exn conn query in
  print_table result;
  print_footer ()

let report_part_handler ~place ~conn id =
  print_header ();
  let query = "SELECT som_id, tc_config_id, som_config_id FROM reports " ^
    (sprintf "WHERE report_part_id=%d" id) in
  let result = exec_query_exn conn query in
  let row = result#get_all.(0) in
  let som_id = int_of_string (row.(0)) in
  let tc_config_id = int_of_string (row.(1)) in
  let som_config_id = if result#getisnull 0 2 then "(NULL)" else row.(2) in
  printf "REPORT PART: %d<br />\n" id;
  printf "SOM ID: %d<br />\n" som_id;
  printf "TC CONFIG ID: %d<br />\n" tc_config_id;
  printf "SOM CONFIG ID: %s<br />\n" som_config_id;
  let url = sprintf "/?som=%d&v_config_id=%d" som_id tc_config_id in
  printf "URL: <a href='%s'>%s</a>" url url;
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

let print_x_axis_choice configs som_configs_opt machines =
  print_select_list
    ~label:"X axis" ~attrs:[("name", "xaxis")]
    (get_xy_choices configs som_configs_opt machines)

let print_y_axis_choice configs som_configs_opt machines =
  print_select_list
    ~label:"Y axis" ~attrs:[("name", "yaxis")]
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

let som_config_tbl_exists conn som_id =
  let som_config_tbl = sprintf "som_config_%d" som_id in
  let query = "SELECT * FROM pg_tables WHERE schemaname='public' AND " ^
    (sprintf "tablename='%s'" som_config_tbl) in
  som_config_tbl, (exec_query_exn conn query)#ntuples = 1

let show_configurations ~conn som_id tc_config_tbl =
  let query =
    sprintf "SELECT * FROM soms WHERE som_id=%d" som_id in
  let som_info = exec_query_exn conn query in
  let query = "SELECT * FROM " ^ tc_config_tbl in
  let configs = exec_query_exn conn query in
  let query = "SELECT DISTINCT job_id FROM measurements" in
  let job_ids = exec_query_exn conn query in
  let query = "SELECT DISTINCT build_id FROM jobs AS j, measurements AS m " ^
    (sprintf "WHERE j.job_id=m.job_id AND som_id=%d" som_id) in
  let build_ids = get_first_col (exec_query_exn conn query) in
  let query = "SELECT branch, build_number, build_tag FROM builds WHERE " ^
    (sprintf "build_id IN (%s)" (concat build_ids)) in
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
  printf "<form name='optionsForm'>\n";
  print_x_axis_choice configs som_configs_opt machines;
  print_y_axis_choice configs som_configs_opt machines;
  let checkbox_prefix = "<input type='checkbox' name=" in
  printf "%s'show_avgs' checked='checked' />Show averages\n" checkbox_prefix;
  printf "%s'yaxis_log' />Log scale Y\n" checkbox_prefix;
  printf "%s'y_from_zero' />Force Y from 0\n" checkbox_prefix;
  printf "%s'show_all_meta' />Show all meta-data<br />\n" checkbox_prefix;
  print_filter_table job_ids builds configs som_configs_opt machines;
  printf "</form>\n";
  let submit_prefix = "<input type='submit' id=" in
  printf "%s'reset_config' value='Reset Configuration' />" submit_prefix;
  printf "%s'get_img' value='Get Image' />" submit_prefix;
  printf "%s'get_tinyurl' value='Get Tiny URL' />" submit_prefix;
  printf "<a id='tinyurl' style='display: none' title='Tiny URL'></a>";
  printf "<br /><img id='progress_img' src='progress.gif' />\n";
  printf "<div id='graph' style='width: 1000px; height: 600px'></div>";
  printf "<script src='rage.js'></script>"

let som_handler ~place ~conn som_id config_ids =
  print_header ();
  let tc_fqn, tc_config_tbl = get_tc_config_tbl_name conn som_id in
  show_configurations ~conn som_id tc_config_tbl;
  print_footer ()

let html_to_text html =
  let rules =
    let pairs = [("%20", " "); ("%2C", ","); ("+", " ")] in
    List.map pairs ~f:(fun (r, t) -> (Str.regexp r, t))
  in
  List.fold_left rules ~init:html
    ~f:(fun h (r, t) -> Str.global_replace r t h)

let extract_filter tc_config_tbl som_config_tbl col_fqns col_types params =
  let m = String.Table.create () in
  let update_m v vs_opt =
    let vs = Option.value vs_opt ~default:[] in Some (v::vs) in
  let filter_insert (k, v) =
    if v = "ALL" then () else
    if String.is_prefix k ~prefix:values_prefix then begin
      let k2 = String.chop_prefix_exn k ~prefix:values_prefix in
      String.Table.change m k2 (update_m v)
    end in
  List.iter params ~f:filter_insert;
  let l = String.Table.to_alist m in
  let conds = List.map l
    ~f:(fun (k, vs) ->
      let vs = List.map vs ~f:html_to_text in
      let has_null = List.mem ~set:vs "(NULL)" in
      let vs = if has_null then List.filter vs ~f:((<>) "(NULL)") else vs in
      let ty_opt = String.Table.find col_types k in
      let quote = match ty_opt with
      | None -> false
      | Some ty -> List.mem ~set:["character varying"; "boolean"; "text"] ty in
      let vs_oq =
        if quote then List.map vs ~f:(fun v -> "'" ^ v ^ "'") else vs in
      let fqn = String.Table.find_exn col_fqns k in
      let val_list = concat vs_oq in
      let in_cond = sprintf "%s IN (%s)" fqn val_list in
      let null_cond = if has_null then sprintf "%s IS NULL" fqn else "" in
      if List.is_empty vs then null_cond else
      if has_null then sprintf "(%s OR %s)" in_cond null_cond else
      in_cond
    ) in
  concat ~sep:" AND " conds

let get_column_types conn tbl =
  let query =
    ("SELECT column_name, data_type FROM information_schema.columns ") ^
    (sprintf "WHERE table_name='%s'" tbl) in
  let col_types_data = exec_query_exn conn query in
  let nameToType = String.Table.create () in
  let process_column nameTypeList =
    let name = List.nth_exn nameTypeList 0 in
    let ty = List.nth_exn nameTypeList 1 in
    String.Table.replace nameToType ~key:name ~data:ty
  in List.iter col_types_data#get_all_lst ~f:process_column;
  nameToType

let get_column_fqns conn tbl =
  let query =
    ("SELECT column_name FROM information_schema.columns ") ^
    (sprintf "WHERE table_name='%s'" tbl) in
  let col_names = get_first_col (exec_query_exn conn query) in
  let nameToFqn = String.Table.create () in
  let process_column name =
    let fqn = tbl ^ "." ^ name in
    String.Table.replace nameToFqn ~key:name ~data:fqn
  in List.iter col_names ~f:process_column;
  nameToFqn

let combine_maps conn tbls f =
  let m = String.Table.create () in
  List.iter tbls ~f:(fun t -> merge_table_into (f conn t) m);
  m
let get_column_types_many conn tbls = combine_maps conn tbls get_column_types
let get_column_fqns_many conn tbls = combine_maps conn tbls get_column_fqns

let extract_column rows col =
  Array.to_list (Array.map rows ~f:(fun row -> row.(col)))

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
  let filter =
    extract_filter tc_config_tbl som_config_tbl col_fqns col_types params in
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
      "\"" ^ (List.nth_exn keys (i+2)) ^ "\":\"" ^ v ^ "\"" in
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
  let select = sprintf "SELECT key FROM tbl_tinyurls WHERE url='%s'" url in
  let r = exec_query_exn conn select in
  match r#ntuples with
  | 1 -> printf "{\"id\":%s}" (r#getvalue 0 0)
  | _ ->
    let insert = sprintf "INSERT INTO tbl_tinyurls (url) VALUES ('%s')" url in
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
  let q = sprintf "SELECT url FROM tbl_tinyurls WHERE key=%d" id in
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
    | Report -> report_handler ~place ~conn
    | ReportPart id -> report_part_handler ~place ~conn id
    | Default -> default_handler ~place ~conn
  end;
  conn#finish

let _ =
  handle_request ()
