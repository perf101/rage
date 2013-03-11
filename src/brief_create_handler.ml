open Core.Std
open Utils

let t ~args = object (self)
  inherit Html_handler.t ~args

  method private get_builds_from_params key tbl =
    let build_vals = List.filter ~f:((<>) "NONE") (self#get_params key) in
    let builds_str =
      if List.mem "ALL" ~set:build_vals
      then
        let result =
          Sql.exec_exn ~conn ~query:("SELECT build_number FROM " ^ tbl) in
        Sql.get_col ~result ~col:0
      else build_vals
    in List.map ~f:int_of_string builds_str

  method private get_build_group group =
    let std_builds = self#get_builds_from_params (group ^ "_standard_builds")
      "standard_builds" in
    let all_builds = self#get_builds_from_params (group ^ "_all_builds")
      "builds" in
    List.sort ~cmp:compare (List.dedup (std_builds @ all_builds))

  (** Given [["a"; "b"]; ["1"; "2"]; ["x"; "y"]], this function outputs
   * [ ["a"; "1"; "x"]; ["a"; "1"; "y"]; ["a"; "2"; "x"]; ["a"; "2"; "y"];
   *   ["b"; "1"; "x"]; ["b"; "1"; "y"]; ["b"; "2"; "x"]; ["b"; "2"; "y"] ]. *)
  method private generate_permutations dimensions =
    let rec explode base = function
      | [] -> [List.rev base]
      | d::ds -> List.concat (List.map ~f:(fun v -> explode (v::base) ds) d)
    in explode [] dimensions

  method handle =
    (* If "id" is specified, then modify report <id>. *)
    let id_opt = self#get_param "id" in
    ignore (Option.map id_opt ~f:(fun id ->
      let query = "DELETE FROM reports WHERE report_id = " ^ id in
      Sql.exec_ign_exn ~conn ~query
    ));
    (* Obtain metadata. *)
    let desc = self#get_param_exn "desc" in
    let xaxis = String.concat ~sep:"," (self#get_params "xaxis") in
    let yaxis = self#get_param_exn "yaxis" in
    let tuples =
      (match id_opt with None -> [] | Some id -> [("report_id", id)]) @
      [("report_desc", desc); ("xaxis", xaxis); ("yaxis", yaxis)]
    in
    let report_id = Sql.ensure_inserted_get_id ~conn ~tbl:"reports" ~tuples in
    (* Gather and record builds. *)
    let primary_builds = self#get_build_group "primary" in
    let secondary_builds = self#get_build_group "secondary" in
    (* XXX note that build_tag is assumed to be empty! *)
    let insert_build ~primary build_number =
      let query = "SELECT build_id FROM builds " ^
        (sprintf "WHERE build_number=%d AND build_tag=''" build_number) in
      match Sql.get_first_entry ~result:(Sql.exec_exn ~conn ~query) with
      | None ->
        if primary then
          failwith (sprintf "Could not find build with build_number %d." build_number)
      | Some build_id ->
        let tuples = [
          ("report_id", string_of_int report_id);
          ("build_id", build_id);
          ("primary", string_of_bool primary);
        ] in
        Sql.ensure_inserted ~conn ~tbl:"report_builds" ~tuples
    in
    List.iter ~f:(insert_build ~primary:true) primary_builds;
    List.iter ~f:(insert_build ~primary:false) secondary_builds;
    (* Find included SOMs. *)
    let param_keys = fst (List.split params) in
    let som_ids_str =
      List.filter_map ~f:(String.chop_prefix ~prefix:"include_som_") param_keys in
    let som_ids = List.map ~f:int_of_string som_ids_str in
    (* Process SOMs. *)
    let graph_number = ref 0 in
    let process_som som_id =
      let tc_fqn =
        let query = sprintf "SELECT tc_fqn FROM soms WHERE som_id=%d" som_id in
        Sql.get_first_entry_exn ~result:(Sql.exec_exn ~conn ~query)
      in
      let tc_prefix = "tc-" ^ tc_fqn ^ "_" in
      let som_prefix = sprintf "som-%d_" som_id in
      let tc_config_tbl = "tc_config_" ^ tc_fqn in
      let col_fqns = get_column_fqns conn tc_config_tbl in
      let col_types = get_column_types conn tc_config_tbl in
      (* Find all permutations for tc/som configs with "split_by_graph". First,
         we find all related tc/som configuration names marked with
         "split_by_graph": split_by_graph_params. Then, we obtain a list of lists
         representing selected values for each configuration. Finally, we invoke
         generate_permutations on the list of lists. *)
      let extract_split_bys (k, v) =
        let is_related =
          String.is_prefix k ~prefix:tc_prefix ||
          String.is_prefix k ~prefix:som_prefix
        in
        match is_related with false -> None | true ->
        match String.is_suffix k ~suffix:"_split" with false -> None | true ->
        Some (k, v)
      in
      let extract_split_by_graph_keys (k, v) =
        match extract_split_bys (k, v) with None -> None | Some (k, v) ->
        if v <> "split_by_graph" then None else
        Some (String.chop_suffix_exn k ~suffix:"_split")
      in
      let split_by_graph_keys =
        List.dedup (List.filter_map ~f:extract_split_by_graph_keys params) in
      let select_split_by_graphs (k, v) = List.mem k ~set:split_by_graph_keys in
      let split_by_graph_params = List.filter ~f:select_split_by_graphs params in
      let split_by_graphs_values = List.map split_by_graph_keys
        ~f:(self#get_params_gen ~params:split_by_graph_params) in
      let split_by_graphs_perms = self#generate_permutations split_by_graphs_values in
      (* Insert "split_by_line"s. *)
      let extract_split_by_line_keys (k, v) =
        match extract_split_bys (k, v) with None -> None | Some (k, v) ->
        if v <> "split_by_line" then None else
        let no_suffix = String.chop_suffix_exn k ~suffix:"_split" in
        Some (String.drop_prefix no_suffix (String.index_exn no_suffix '_' + 1))
      in
      let split_by_line_keys =
        List.dedup (List.filter_map ~f:extract_split_by_line_keys params) in
      let insert_split plot_id ty property =
        let tuples = [
          ("plot_id", string_of_int plot_id);
          ("property", property);
          ("type", ty);
        ] in
        Sql.ensure_inserted ~conn ~tbl:"report_plot_split_bys" ~tuples
      in
      (* Remove all "_split" params. *)
      let params =
        List.filter ~f:(fun kv -> Option.is_none (extract_split_bys kv)) params in
      (* Remove all keys found in permutations. *)
      let is_not_permutation_pair (k, _) =
        not (List.mem k ~set:split_by_graph_keys) in
      let params = List.filter ~f:is_not_permutation_pair params in
      (* For each such permutation.. *)
      let process_permutation permutation =
        let extra_params = List.combine_exn split_by_graph_keys permutation in
        let params = params @ extra_params in
        let filter = extract_filter col_fqns col_types params tc_prefix in
        let query =
          (sprintf "SELECT tc_config_id FROM %s" tc_config_tbl) ^
          (if filter = "" then "" else sprintf " WHERE %s" filter) in
        let tc_config_ids_str =
          Sql.get_col ~result:(Sql.exec_exn ~conn ~query) ~col:0 in
        let tc_config_ids = List.map ~f:int_of_string tc_config_ids_str in
        match tc_config_ids with [] -> () | _ ->
        (* Get a new plot ID. *)
        let tuples = [
          ("report_id", string_of_int report_id);
          ("graph_number", string_of_int !graph_number);
          ("som_id", string_of_int som_id);
        ] in
        incr graph_number;
        let plot_id =
          Sql.ensure_inserted_get_id ~conn ~tbl:"report_plots" ~tuples in
        (* Insert TC configs for the plot ID. *)
        let insert_tc_config_id tc_config_id =
          let tuples = [
            ("plot_id", string_of_int plot_id);
            ("tc_config_id", string_of_int tc_config_id)
          ] in
          Sql.ensure_inserted ~conn ~tbl:"report_plot_tc_configs" ~tuples
        in List.iter ~f:insert_tc_config_id tc_config_ids;
        (* Insert split-bys for the plot ID. *)
        List.iter ~f:(insert_split plot_id "graph") split_by_graph_keys;
        List.iter ~f:(insert_split plot_id "line") split_by_line_keys;
        (* Insert SOM configs for the plot ID (if required). *)
        match som_config_tbl_exists ~conn som_id with
        | som_config_tbl, true ->
            let col_fqns = get_column_fqns conn som_config_tbl in
            let col_types = get_column_types conn som_config_tbl in
            let filter = extract_filter col_fqns col_types params som_prefix in
            let query = sprintf "SELECT som_config_id FROM %s " som_config_tbl ^
              (if filter = "" then "" else sprintf "WHERE %s" filter) in
            let som_config_ids_str =
              Sql.get_col ~result:(Sql.exec_exn ~conn ~query) ~col:0 in
            let som_config_ids = List.map ~f:int_of_string som_config_ids_str in
            let insert_som_config_id som_config_id =
              let tuples = [
                ("plot_id", string_of_int plot_id);
                ("som_config_id", string_of_int som_config_id)
              ] in
              Sql.ensure_inserted ~conn ~tbl:"report_plot_som_configs" ~tuples
            in List.iter ~f:insert_som_config_id som_config_ids
        | _ -> ()
      in
      List.iter ~f:process_permutation split_by_graphs_perms;
    in
    List.iter ~f:process_som som_ids;
    self#javascript_redirect "/?p=reports"
end
