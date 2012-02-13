open Core.Std
open Printf
open Postgresql
open Utils

module ListKey = struct
  module T = struct
    type t = string list with sexp
    type sexpable = t
    let compare = compare
    let equal = (=)
    let hash = Hashtbl.hash
  end
  include T
  include Hashable.Make (T)
end

let debug msg = output_string stderr (msg ^ "\n")

type place =
  | Default
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

let values_for_key pairs key =
  List.fold pairs ~init:[]
    ~f:(fun acc (k, v) -> if k = key then v::acc else acc)

let place_of_request req =
  let open List.Assoc in
  let pairs = pairs_of_request req in
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

let exec_query ~(conn : connection) (query : string) : result option =
  let result = conn#exec query in
  match result#status with
    | Command_ok | Tuples_ok -> Some result
    | _ -> print_endline conn#error_message; None

let default_handler ~place ~conn =
  print_header ();
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
  print_table_custom_row print_row result;
  print_footer ()

let get_tbl_names ~conn som_id =
  let query = "SELECT tc FROM tbl_som_definitions " ^
              "WHERE som_id = " ^ (string_of_int som_id) in
  match exec_query ~conn query with None -> None | Some result ->
  let tc = String.lowercase (result#getvalue 0 0) in
  let config_tbl = "tbl_config_tc_" ^ tc in
  let jobs_tbl = "tbl_jobs_tc_" ^ tc in
  Some (config_tbl, jobs_tbl)

let get_build_mapping ~conn builds =
  let builds_uniq = List.dedup ~compare builds in
  let query =
    "SELECT name FROM tbl_versions WHERE name IN ('" ^
    (concat ~sep:"','" builds_uniq) ^ "') ORDER BY seq_no" in
  match exec_query ~conn query with None -> None | Some data ->
  let process_row i row = (i+1, List.nth_exn row 0) in
  Some (List.mapi data#get_all_lst ~f:process_row)

let print_som_info som_info =
  let i = som_info#get_all.(0) in
  let name = sprintf "<span class='som_name'>%s</span>" i.(2) in
  let prefix = "<div class='som'>SOM:" in
  let suffix = "</div><br />\n" in
  printf "%s %s (id: %s, tc: %s) %s" prefix name i.(0) i.(3) suffix

let get_xy_choices configs =
  "build" :: configs#get_fnames_lst

let print_x_axis_choice configs =
  print_select_list
    ~label:"X axis" ~attrs:[("name", "xaxis")] (get_xy_choices configs)

let print_y_axis_choice configs =
  print_select_list
    ~label:"Y axis" ~attrs:[("name", "yaxis")]
    ("result" :: (get_xy_choices configs))

let filter_prefix = "f_"
let values_prefix = "v_"
let show_for_value = "0"
let filter_by_value = "1"

let print_filter_table configs builds =
  let nRows = configs#ntuples - 1 in
  let nCols = configs#nfields in
  let labels = "build" :: configs#get_fnames_lst in
  print_endline "  <table border='1' class='filter_table'>\n";
  print_row_default (-1) "th" labels;
  printf "<tr>\n";
  for col = 0 to nCols do
    let name = filter_prefix ^ (List.nth_exn labels col) in
    print_select ~td:true
      ~attrs:[("name", name); ("class", "filterselect")]
      [("SHOW FOR", show_for_value); ("SPLIT BY", filter_by_value)]
  done;
  printf "</tr><tr>\n";
  let build_lst = get_options_for_field
    builds (builds#ntuples-1) 0 (builds#ftype 0) in
  let options_lst = List.map (List.range 0 nCols) ~f:(fun col ->
    get_options_for_field configs nRows col (configs#ftype col)) in
  let options_lst = build_lst :: options_lst in
  let print_td_multiselect col options =
    let name = values_prefix ^ (List.nth_exn labels col) in
    print_select_list ~td:true ~selected:["ALL"]
      ~attrs:[("name", name); ("multiple", "multiple");
              ("size", "3"); ("class", "multiselect")]
      ("ALL"::options) in
  List.iteri options_lst ~f:print_td_multiselect;
  printf "</tr></table>\n"

let show_configurations ~conn som_id config_tbl jobs_tbl =
  let query =
    sprintf "SELECT * FROM tbl_som_definitions WHERE som_id=%d" som_id in
  match exec_query ~conn query with None -> () | Some som_info ->
  let query = "SELECT * FROM " ^ config_tbl in
  match exec_query ~conn query with None -> () | Some configs ->
  let query = "SELECT DISTINCT build FROM " ^ jobs_tbl ^ " ORDER BY build" in
  match exec_query ~conn query with None -> () | Some builds ->
  print_som_info som_info;
  printf "<form name='optionsForm'>\n";
  print_x_axis_choice configs;
  print_y_axis_choice configs;
  let checkbox_prefix = "<input type='checkbox' name=" in
  printf "%s'show_avgs' checked='checked' />Show averages\n" checkbox_prefix;
  printf "%s'yaxis_log' />Log scale Y<br />\n" checkbox_prefix;
  print_filter_table configs builds;
  printf "</form>\n";
  printf "<input type='submit' id='reset_config' value='Reset Configuration' />";
  printf "<input type='submit' id='get_img' value='Get Image' />";
  printf "<input type='submit' id='get_tinyurl' value='Get Tiny URL' />";
  printf "<a id='tinyurl' style='display: none' title='Tiny URL'></a>";
  printf "<br /><img id='progress_img' src='progress.gif' />\n";
  printf "<div id='graph' style='width: 1000px; height: 600px'></div>";
  printf "<script src='rage.js'></script>"

let som_handler ~place ~conn som_id config_ids =
  print_header ();
  match get_tbl_names ~conn som_id
  with None -> () | Some (config_tbl, jobs_tbl) ->
  show_configurations ~conn som_id config_tbl jobs_tbl;
  print_footer ()

let get_fqn_for_field config_tbl field =
  match field with
  | "build" | "result" -> "tbl_measurements." ^ field
  | _ -> config_tbl ^ "." ^ field

let extract_filter config_tbl col_types params =
  let m = String.Table.create () in
  let update_m v vs_opt =
    let vs = Option.value vs_opt ~default:[] in Some (v::vs) in
  let filter_insert (k, v) =
    if v = "ALL" then () else
    if String.is_prefix k ~prefix:values_prefix then begin
      let k2 = String.chop_prefix_exn k ~prefix:values_prefix in
      String.Table.change m k2 (update_m v)
    end in
  List.iter params filter_insert;
  let l = String.Table.to_alist m in
  let conds = List.map l
    ~f:(fun (k, vs) ->
      let has_null = List.mem ~set:vs "(NULL)" in
      let vs = if has_null then List.filter vs ~f:((<>) "(NULL)") else vs in
      let ty_opt = String.Table.find col_types k in
      let quote = match ty_opt with
      | None -> false
      | Some ty -> List.mem ~set:["character varying"; "boolean"; "text"] ty in
      let vs_oq =
        if quote then List.map vs ~f:(fun v -> "'" ^ v ^ "'") else vs in
      let fqn = get_fqn_for_field config_tbl k in
      let val_list = concat vs_oq in
      let in_cond = sprintf "%s IN (%s)" fqn val_list in
      let null_cond = if has_null then sprintf "%s IS NULL" fqn else "" in
      if List.is_empty vs then null_cond else
      if has_null then sprintf "(%s OR %s)" in_cond null_cond else
      in_cond
    ) in
  concat ~sep:" AND " conds

let get_column_types ~conn tbl =
  let query =
    ("SELECT column_name, data_type FROM information_schema.columns ") ^
    (sprintf "WHERE table_name='%s'" tbl) in
  match exec_query ~conn query with None -> None | Some col_types_data ->
  let nameToType = String.Table.create () in
  let process_column nameTypeList =
    let name = List.nth_exn nameTypeList 0 in
    let ty = List.nth_exn nameTypeList 1 in
    String.Table.replace nameToType ~key:name ~data:ty 
  in List.iter col_types_data#get_all_lst ~f:process_column;
  Some nameToType

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
    if col_name = "build"
    then get_build_mapping ~conn (extract_column rows col)
    else get_generic_string_mapping rows col col_name col_types
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

let asyncsom_handler ~conn som_id params =
  printf "Content-type: application/json\n\n";
  match get_tbl_names ~conn som_id
  with None -> () | Some (config_tbl, jobs_tbl) ->
  (* determine filter columns and their types *)
  match get_column_types ~conn config_tbl with None -> () | Some col_types ->
  String.Table.replace col_types ~key:"build" ~data:"character varying";
  let get_first_val k d =
    Option.value ~default:d (List.hd (values_for_key params k)) in
  let xaxis = get_first_val "xaxis" "build" in
  let yaxis = get_first_val "yaxis" "result" in
  let x_fqn = get_fqn_for_field config_tbl xaxis in
  let y_fqn = get_fqn_for_field config_tbl yaxis in
  let keys = String.Table.keys col_types in
  let other_keys = List.filter keys ~f:(fun x -> x <> xaxis && x <> yaxis) in
  let ordered_keys = xaxis :: yaxis :: other_keys in
  let other_fqns = List.map other_keys ~f:(get_fqn_for_field config_tbl) in
  let fqns = concat other_fqns in
  let filter = extract_filter config_tbl col_types params in
  (* obtain data from database *)
  let query =
    (sprintf "SELECT %s,%s" x_fqn y_fqn) ^
    (if not (String.is_empty fqns) then sprintf ",%s " fqns else " ") ^
    (sprintf "FROM %s INNER JOIN tbl_measurements " config_tbl) ^
    (sprintf "ON %s.config_id=tbl_measurements.config_id " config_tbl) ^
    (sprintf "WHERE tbl_measurements.som_id=%d" som_id) ^
    (if not (String.is_empty filter) then sprintf " AND %s" filter else "") in
  match exec_query ~conn query with None -> () | Some data ->
  let rows = data#get_all in
  (* filter data into groups based on "FILTER BY"-s *)
  let filter_map_split_bys (k, v) =
    if String.is_prefix k filter_prefix && v = filter_by_value
    then String.chop_prefix k ~prefix:filter_prefix else None in
  let split_bys = List.filter_map params ~f:filter_map_split_bys in
  let split_by_is = List.map split_bys ~f:(index ordered_keys) in
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
  strings_to_numbers ~conn rows 0 xaxis col_types "x_labels";
  strings_to_numbers ~conn rows 1 yaxis col_types "y_labels";
  let num_other_keys = List.length keys - 2 in
  let convert_row row =
    let other_vals = Array.sub row ~pos:2 ~len:num_other_keys in
    let process_val i v =
      "\"" ^ (List.nth_exn other_keys i) ^ "\":\"" ^ v ^ "\"" in
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
  match exec_query ~conn select with None -> () | Some r ->
  match r#ntuples with
  | 1 -> printf "{\"id\":%s}" (r#getvalue 0 0)
  | _ ->
    let insert = sprintf "INSERT INTO tbl_tinyurls (url) VALUES ('%s')" url in
    match exec_query ~conn insert with None -> () | Some _ ->
    match exec_query ~conn select with None -> () | Some r ->
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
  match exec_query ~conn q with | None -> () | Some r ->
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
    | Default -> default_handler ~place ~conn
  end;
  conn#finish

let _ =
  handle_request ()
