open! Core.Std

let debug msg =
  output_string stderr (msg ^ "\n");
  flush stderr

let index l x =
  let rec aux i = function
    | [] -> failwith "index []"
    | x'::xs -> if x = x' then i else aux (i+1) xs
  in aux 0 l

let concat ?(sep = ",") l =
  String.concat ~sep
    (List.filter l ~f:(fun s -> not (String.is_empty s)))

let string_of_int_list is =
  concat (List.map ~f:string_of_int is)

let rec print_concat ?(sep = ",") = function
  | [] -> ()
  | [e] -> print_string e
  | e::l -> print_string e; print_string sep; print_concat l

let concat_array ?(sep = ",") a =
  String.concat_array ~sep
    (Array.filter a ~f:(fun s -> not (String.is_empty s)))

let merge_table_into src dst =
  String.Table.merge_into ~src ~dst
    ~f:(fun ~key:_ src_v dst_v_opt ->
      match dst_v_opt with None -> Some src_v | vo -> vo)

let cat filename =
  print_string (In_channel.with_file ~f:In_channel.input_all filename)

(* DATABASE INTERACTION *)

let get_value r row col null_val =
  if r#getisnull row col then null_val else r#getvalue row col

let combine_maps conn tbls f =
  let m = String.Table.create () in
  List.iter tbls ~f:(fun t -> merge_table_into (f conn t) m);
  m

let get_column_types conn tbl =
  String.Table.of_alist_exn (Sql.get_col_types_lst ~conn ~tbl)

let get_column_types_many conn tbls = combine_maps conn tbls get_column_types

let get_column_fqns conn tbl =
  let col_names = Sql.get_col_names ~conn ~tbl in
  let nameToFqn = String.Table.create () in
  let process_column name =
    let fqn = tbl ^ "." ^ name in
    String.Table.replace nameToFqn ~key:name ~data:fqn
  in List.iter col_names ~f:process_column;
  nameToFqn

let get_column_fqns_many conn tbls = combine_maps conn tbls get_column_fqns

let hex_of_char c =
  let i = int_of_char c in
  if i < int_of_char 'A'
  then i - (int_of_char '0')
  else i - (int_of_char 'A') + 10

let decode_html s =
  let s = Str.global_replace (Str.regexp "+") " " s in
  let re = Str.regexp "%[0-9A-F][0-9A-F]" in
  let rec aux s start len =
    try
      let b = Str.search_forward re s start in
      let e = Str.match_end () in
      let x, y = String.get s (b+1), String.get s (b+2) in
      let x_h, y_h = hex_of_char x, hex_of_char y in
      let c = char_of_int (x_h * 16 + y_h) in
      let prefix = String.sub s ~pos:0 ~len:b in
      let suffix = String.sub s ~pos:e ~len:(len - e) in
      let s = prefix ^ (String.make 1 c) ^ suffix in
      aux s (b + 1) (len - 2)
    with Not_found -> s
  in
  aux s 0 (String.length s)

let extract_filter col_fqns col_types params key_prefix =
  let m = String.Table.create () in
  let update_m v vs_opt =
    let vs = Option.value vs_opt ~default:[] in Some (v::vs) in
  let filter_insert (k, v) =
    if v = "ALL" then () else
    if String.is_prefix k ~prefix:key_prefix then begin
      let k2 = String.chop_prefix_exn k ~prefix:key_prefix in
      String.Table.change m k2 (update_m v)
    end in
  List.iter params ~f:filter_insert;
  let l = String.Table.to_alist m in
  let conds = List.map l
    ~f:(fun (k, vs) ->
      let vs = List.map vs ~f:decode_html in
      let has_null = List.mem vs "(NULL)" in
      let vs = if has_null then List.filter vs ~f:((<>) "(NULL)") else vs in
      let ty = String.Table.find_exn col_types k in
      let quote = Sql.Type.is_quoted ty in
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

(* PRINTING HTML *)

let print_select ?(td=false) ?(label="") ?(selected=[]) ?(attrs=[]) options =
  if td then printf "<td>\n";
  if label <> "" then printf "<b>%s</b>:\n" label;
  printf "<select";
  List.iter attrs ~f:(fun (k, v) -> printf " %s='%s'" k v);
  printf ">\n";
  let print_option (l, v) =
    printf "<option value='%s'" v;
    if List.mem selected l then printf " selected='selected'";
    printf ">%s</option>\n" l
  in List.iter options ~f:print_option;
  printf "</select>\n";
  if td then printf "</td>\n"

let print_select_list ?(td=false) ?(label="") ?(selected=[]) ?(attrs=[]) l =
  print_select ~td ~label ~selected ~attrs (List.map l ~f:(fun x -> (x, x)))

let get_options_for_field db_result ~data col =
  let nRows = db_result#ntuples - 1 in
  let ftype = db_result#ftype col in
  let rec aux acc = function
    | -1 -> acc
    | i ->
      let elem =
        if db_result#getisnull i col then "(NULL)" else data.(i).(col)
      in aux (elem::acc) (i-1)
  in
  let cmp x y =
    try
      if ftype = Postgresql.INT4
      then compare (int_of_string x) (int_of_string y)
      else compare x y
    with _ -> 0
  in
  List.sort ~cmp (List.dedup (aux [] nRows))

let get_options_for_field_once db_result col =
  let data = db_result#get_all in
  get_options_for_field db_result ~data col

let get_options_for_field_once_byname db_result col_name =
  let col_names = db_result#get_fnames_lst in
  let col = match List.findi ~f:(fun _ c -> c = col_name) col_names with
      | Some (i, _) -> i
      | _ -> failwith (sprintf "could not find column '%s' amongst [%s]" col_name (String.concat ~sep:"; " col_names))
  in
  let data = db_result#get_all in
  get_options_for_field db_result ~data col

let print_options_for_field namespace db_result col =
  let fname = db_result#fname col in
  let opts = get_options_for_field_once db_result col in
  let form_name = sprintf "%s_%s" namespace fname in
  printf "<table border='1' class='filter_table'>";
  printf "<tr><th>%s</th></tr><tr>" fname;
  print_select_list ~td:true ~selected:["ALL"]
    ~attrs:[("name", form_name); ("multiple", "multiple"); ("size", "3");
            ("class", "multiselect")]
    ("ALL"::opts);
  printf "</tr><tr>";
  print_select ~td:true ~selected:["SPLIT_BY_GRAPH"]
    ~attrs:[("name", form_name ^ "_split")]
    [("DON'T SPLIT", "dont_split"); ("SPLIT BY GRAPH", "split_by_graph");
     ("SPLIT BY LINE", "split_by_line")];
  printf "</tr></table>"

let print_options_for_fields conn tbl namespace =
  let query = "SELECT * FROM " ^ tbl in
  let result = Sql.exec_exn ~conn ~query in
  List.iter ~f:(print_options_for_field namespace result)
    (List.range 1 result#nfields);
  printf "<br style='clear: both' />\n"

(* RAGE-specific helper methods. *)

let filter_prefix = "f_"
let filter_by_value = "1"
let values_prefix = "v_"

(* Names of fields in the tc_config table *)
let tc_config_fields = [
  "dom0_memory_static_max";
  "dom0_memory_target";
  "cc_restrictions";
  "redo_log";
  "network_backend";
  "option_clone_on_boot";
  "force_non_debug_xen";
  "cpufreq_governor";
  "xen_cmdline";
  "kernel_cmdline";
  "xenrt_pq_name";
  "xenrt_version";
  "xenrt_internal_version";
  "xenrt_pq_version";
  "dom0_vcpus";
  "host_pcpus";
  "live_patching";
  "host_type";
]

let build_fields = [
  "product";
  "branch";
  "build_number";
  "build_date";
  "build_tag";
]

let job_fields = [
  "job_id";
]

let som_config_tbl_exists ~conn som_id =
  let som_config_tbl = sprintf "som_config_%d" som_id in
  som_config_tbl, Sql.tbl_exists ~conn ~tbl:som_config_tbl

let get_std_xy_choices ~conn =
  let machine_field_lst =
    List.tl_exn (Sql.get_col_names ~conn ~tbl:"machines") in
  job_fields @ build_fields @ tc_config_fields @ machine_field_lst

let get_xy_choices ~conn configs som_configs_opt =
  let som_configs_lst = match som_configs_opt with
    | None -> []
    | Some som_configs -> List.tl_exn som_configs#get_fnames_lst
  in get_std_xy_choices ~conn @ configs#get_fnames_lst @ som_configs_lst

let print_axis_choice ?(multiselect=false) label id choices =
  printf "<div id='%s' style='display: inline-block'>\n" id;
  let attrs = [("name", id)] in
  let attrs = (if multiselect then ("multiple", "multiple")::attrs else attrs) in
  print_select_list ~label ~attrs:attrs choices;
  printf "</div>\n"

let print_empty_x_axis_choice ~conn =
  print_axis_choice "X axis" "xaxis" [] ~multiselect:true

let print_empty_y_axis_choice ~conn =
  print_axis_choice "Y axis" "yaxis" []

let print_x_axis_choice ~conn configs som_configs_opt =
  print_axis_choice "X axis" "xaxis" ~multiselect:true
    (get_xy_choices ~conn configs som_configs_opt)

let print_y_axis_choice ~conn configs som_configs_opt =
  print_axis_choice "Y axis" "yaxis"
    ("result" :: (get_xy_choices ~conn configs som_configs_opt))

let get_tc_config_tbl_name conn som_id =
  let query = "SELECT tc_fqn FROM soms " ^
              "WHERE som_id = " ^ (string_of_int som_id) in
  let result = Sql.exec_exn ~conn ~query in
  let tc_fqn = String.lowercase (result#getvalue 0 0) in
  (tc_fqn, "tc_config_" ^ tc_fqn)

(* WEBSERVER INTERACTION *)

let server_name () =
  (* We use HTTP_HOST, which comes from the client, rather than SERVER_NAME,
   * which is defined by the webserver, in case it contains a port number *)
  Sys.getenv_exn "HTTP_HOST"
