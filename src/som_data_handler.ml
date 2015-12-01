open Core.Std
open Fn
open Utils

(* The maximum number of database rows we are prepared to allow the database to return *)
let limit_rows = 100000

let t ~args = object (self)
  inherit Json_handler.t ~args

  val quote_re = Str.regexp "\""

  method private escape_quotes = Str.global_replace quote_re "\\\""

  method private values_for_key ?(default=[]) key =
    let xs = List.fold params ~init:[]
      ~f:(fun acc (k, v) -> if k = key then v::acc else acc) in
    if xs = [] then default else xs

  method private get_first_val k d =
    Option.value ~default:d (List.hd (self#values_for_key k))

  method private select_params ?(value=None) prefix =
    List.filter_map params ~f:(fun (k, v) ->
      if String.is_prefix k ~prefix && (Option.is_none value || Some v = value)
      then String.chop_prefix k ~prefix else None
    )

  method private get_filter_params = self#select_params filter_prefix

  method private get_values_params = self#select_params values_prefix

  method private get_relevant_params =
    self#get_filter_params @ self#get_values_params

  method private should_sort_alphabetically col_types col_name force_as_seq force_as_num =
    match col_name with
    | [col_name] ->
        begin
          match force_as_seq with true -> true | false ->
          match force_as_num with true -> false | false ->
          match String.Table.find col_types col_name with None -> false | Some ty ->
          Sql.Type.is_quoted ty
        end
    | _ -> (* always quote if it's multiple-key value *) true

  method private get_generic_string_mapping rows col col_name col_types
      force_as_seq force_as_num =
    let sort_seq_numeric a b =
      if force_as_seq then
        (* sort function that sorts integers as integers, everything else as strings *)
        try
          compare (int_of_string a) (int_of_string b)
        with Failure _ ->
          compare a b
      else
        compare a b
    in

    if not (self#should_sort_alphabetically col_types col_name force_as_seq force_as_num)
    then None else
    let col_data = Array.to_list (Array.map ~f:(fun row -> row.(col)) rows) in
    let uniques = List.dedup col_data in
    let sorted = List.sort ~cmp:sort_seq_numeric uniques in
    Some (List.mapi sorted ~f:(fun i x -> (i+1, x)))

  method private strings_to_numbers rows col col_name col_types label
      force_as_seq force_as_num =
    let mapping_opt =
      self#get_generic_string_mapping rows col col_name col_types force_as_seq force_as_num in
    match mapping_opt with None -> () | Some mapping ->
    let process_entry (i, a) = sprintf "\"%d\":\"%s\"" i (self#escape_quotes a) in
    let mapping_str = concat (List.map mapping ~f:process_entry) in
    printf "\"%s\":{%s}," label mapping_str;
    let i_to_string_map = List.Assoc.inverse mapping in
    let i_from_string row =
      match List.Assoc.find i_to_string_map row.(col) with
      | None -> failwith ("NOT IN TRANSLATION MAP: " ^ row.(col) ^ "\n")
      | Some i -> row.(col) <- string_of_int i
    in Array.iter rows ~f:i_from_string

  method private write_body =
    let som_id = int_of_string (self#get_param_exn "id") in
    let tc_fqn, tc_config_tbl = get_tc_config_tbl_name conn som_id in
    let som_config_tbl, som_tbl_exists = som_config_tbl_exists ~conn som_id in
    (* determine filter columns and their types *)
    let tbls = ["measurements_2"; "soms_jobs"; "jobs"; "builds"; "tc_config"; "machines";
      tc_config_tbl] @
      (if som_tbl_exists then [som_config_tbl] else []) in
    let col_fqns = get_column_fqns_many conn tbls in
    let col_types = get_column_types_many conn tbls in
    (* Get axes selections. xaxis may be multi-valued; yaxis is single value. *)
    let xaxis = self#values_for_key "xaxis" ~default:["branch"] in
    (* xaxis could be ["one"; "two"] or ["one%2Ctwo"] -- both are equivalent *)
    let xaxis = List.concat (List.map ~f:(Str.split (Str.regexp "%2C")) xaxis) in
    let yaxis = self#get_first_val "yaxis" "result" in
    let compose_keys ~xaxis ~yaxis ~rest =
      let deduped = List.stable_dedup rest in
      let filter_cond = non (List.mem (yaxis::xaxis)) in
      List.filter ~f:filter_cond deduped
    in
    let restkeys =
      let rest = match self#get_first_val "show_all_meta" "off" with
      | "on" -> String.Table.keys col_fqns
      | _ -> self#get_relevant_params
      in compose_keys ~xaxis ~yaxis ~rest
    in
    let fully_qualify_col = String.Table.find_exn col_fqns in
    let xaxisfqns = List.map xaxis ~f:fully_qualify_col in
    let yaxisfqns = fully_qualify_col yaxis in
    let restfqns = List.map restkeys ~f:fully_qualify_col in
    let xaxis_str = String.concat ~sep:"," xaxis in
    let keys = xaxis_str :: [yaxis] @ xaxis @ restkeys in
    let filter = extract_filter col_fqns col_types params values_prefix in
    (* obtain SOM meta-data *)
    let query = sprintf "SELECT positive FROM soms WHERE som_id=%d" som_id in
    let metadata = Sql.exec_exn ~conn ~query in
    let positive = (Sql.get_first_entry_exn ~result:metadata) = "t" in
    (* obtain data from database *)
    let query =
      "SELECT " ^
      (String.concat ~sep:"||','||" xaxisfqns) ^ ", " ^ (* x-axis *)
      yaxisfqns ^ ", " ^ (* y-axis *)
      (String.concat ~sep:", " xaxisfqns) ^ (* components of x-axis, needed in case we split by one of them *)
      (if restfqns = [] then " " else sprintf ", %s " (String.concat ~sep:", " restfqns)) ^
      (sprintf "FROM %s " (String.concat ~sep:", " tbls)) ^
      (sprintf "WHERE measurements_2.tc_config_id=%s.tc_config_id "
               tc_config_tbl) ^
      (sprintf "AND soms_jobs.som_id=%d " som_id) ^
      "AND soms_jobs.job_id=jobs.job_id " ^
      "AND measurements_2.som_job_id=soms_jobs.id "^
      "AND jobs.build_id=builds.build_id " ^
      "AND tc_config.job_id=jobs.job_id " ^
      (sprintf "AND tc_config.tc_fqn='%s' " tc_fqn) ^
      "AND tc_config.tc_config_id=measurements_2.tc_config_id " ^
      "AND tc_config.machine_id=machines.machine_id" ^
      (if som_tbl_exists
       then sprintf " AND measurements_2.som_config_id=%s.som_config_id"
            som_config_tbl else "") ^
      (if not (String.is_empty filter) then sprintf " AND %s" filter else "") ^
      (sprintf " LIMIT %d" limit_rows)
    in
    let data = Sql.exec_exn ~conn ~query in
    let rows = data#get_all in
    debug (sprintf "The query returned %d rows" (Array.length rows));
    (if Array.length rows = limit_rows then debug (sprintf "WARNING: truncation of data -- we are only returning the first %d rows" limit_rows));
    (* filter data into groups based on "SPLIT BY"-s *)
    let split_bys =
      self#select_params filter_prefix ~value:(Some filter_by_value) in
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
    printf "\"positive\":%B," positive;
    printf "\"target\":\"%s\"," (self#get_first_val "target" "graph");
    printf "\"part\":%s," (self#get_first_val "part" "1");
    printf "\"xaxis\":\"%s\"," xaxis_str;
    printf "\"yaxis\":\"%s\"," yaxis;
    let x_as_seq = ("on" = self#get_first_val "x_as_seq" "off") in
    let y_as_seq = ("on" = self#get_first_val "y_as_seq" "off") in
    let x_as_num = ("on" = self#get_first_val "x_as_num" "off") in
    self#strings_to_numbers rows 0 xaxis col_types "x_labels" x_as_seq x_as_num;
    self#strings_to_numbers rows 1 [yaxis] col_types "y_labels" y_as_seq false;
    let num_other_keys = List.length keys - 2 in
    let convert_row row =
      let other_vals = Array.sub row ~pos:2 ~len:num_other_keys in
      let process_val i v =
        let v' = self#escape_quotes v in
        "\"" ^ (List.nth_exn keys (i+2)) ^ "\":\"" ^ v' ^ "\"" in
      let prop_array = Array.mapi other_vals ~f:process_val in
      let props = concat_array prop_array in
      "[" ^ row.(0) ^ "," ^ row.(1) ^ ",{" ^ props ^ "}]"
    in
    printf "\"series\":[";
    let max_val_length = 40 in
    let process_series i (row_key, rows) =
      let pairs = List.map2_exn split_bys row_key ~f:(fun k v ->
        (* Truncate the value if it's too long *)
        let v = if String.length v > max_val_length
          then (Str.string_before v max_val_length) ^ "..."
          else v
        in
        k ^ "=" ^ v) in
      let label = concat pairs in
      let data_lst = List.map rows ~f:convert_row in
      if i <> 0 then printf ",";
      printf "{\"label\":\"%s\",\"color\":%d,\"data\":[" (self#escape_quotes label) i;
      print_concat data_lst;
      printf "]}"
    in
    List.iteri (ListKey.Table.to_alist all_series) ~f:process_series;
    printf "]}"
end
