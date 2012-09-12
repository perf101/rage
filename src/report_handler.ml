open Core.Std
open Utils

let t ~args = object (self)
  inherit Json_handler.t ~args

  method private write_body =
    let report_id = int_of_string (List.Assoc.find_exn params "id") in
    let query = "SELECT report_desc, xaxis, yaxis " ^
      sprintf "FROM reports WHERE report_id=%d" report_id in
    let report = (Sql.exec_exn ~conn ~query)#get_all.(0) in
    let desc, xaxis, yaxis = report.(0), report.(1), report.(2) in
    let builds_query primary =
      "SELECT b.build_id, b.branch, b.build_number, b.build_tag " ^
      "FROM builds AS b, report_builds AS rb " ^
      "WHERE b.build_id = rb.build_id " ^
      (sprintf "AND rb.report_id = %d " report_id) ^
      (sprintf "AND rb.primary = %B " primary) in
    let primary_builds = Sql.exec_exn ~conn ~query:(builds_query true) in
    let secondary_builds = Sql.exec_exn ~conn ~query:(builds_query false) in
    let string_of_builds builds =
      let string_of_build build =
        "{" ^
        (sprintf "\"build_id\": %s," build.(0)) ^
        (sprintf "\"branch\": \"%s\"," build.(1)) ^
        (sprintf "\"build_number\": %s," build.(2)) ^
        (sprintf "\"build_tag\": \"%s\"" build.(3)) ^
        "}"
      in
      concat (Array.to_list (Array.map ~f:string_of_build builds#get_all))
    in
    let query =
      "SELECT plot_id, graph_number, som_id FROM report_plots " ^
      (sprintf "WHERE report_id = %d" report_id) in
    let report_plots = Sql.exec_exn ~conn ~query in
    let string_of_report_plot plot =
      let plot_id : int = int_of_string plot.(0) in
      (* let graph_number : int = int_of_string plot.(1) in *)
      let som_id : int = int_of_string plot.(2) in
      (* Obtain TC and SOM metadata. *)
      let query = "SELECT som_name, tc_fqn, more_is_better, units FROM soms " ^
        (sprintf "WHERE som_id = %d" som_id) in
      let som_info = Sql.exec_exn ~conn ~query in
      let som_name = som_info#getvalue 0 0 in
      let tc_fqn = som_info#getvalue 0 1 in
      let more_is_better = get_value som_info 0 2 "NULL" in
      let units = get_value som_info 0 3 "NULL" in
      let query = "SELECT description FROM test_cases " ^
        (sprintf "WHERE tc_fqn = '%s'" tc_fqn) in
      let tc_desc =
        Sql.get_first_entry_exn ~result:(Sql.exec_exn ~conn ~query) in
      (* Obtain TC/SOM config IDs and split-by-lines for this plot. *)
      let query = "SELECT tc_config_id FROM report_plot_tc_configs " ^
        sprintf "WHERE plot_id = %d" plot_id in
      let result = Sql.exec_exn ~conn ~query in
      let tc_config_ids =
        List.map ~f:int_of_string (Sql.get_col ~result ~col:0) in
      let query = "SELECT som_config_id FROM report_plot_som_configs " ^
        sprintf "WHERE plot_id = %d" plot_id in
      let result = Sql.exec_exn ~conn ~query in
      let som_config_ids =
        List.map ~f:int_of_string (Sql.get_col ~result ~col:0) in
      let query = "SELECT property, type FROM report_plot_split_bys " ^
        sprintf "WHERE plot_id = %d" plot_id in
      let split_bys = Sql.exec_exn ~conn ~query in
      (* Obtain config pairs for config IDs. *)
      let string_of_config_info_part config_info col =
        let fname = config_info#fname col in
        let value = config_info#getvalue 0 col in
        sprintf "\"%s\": \"%s\"" fname value
      in
      let string_of_tc_config tc_config_id =
        let query =
          (sprintf "SELECT * FROM tc_config_%s " tc_fqn) ^
          (sprintf "WHERE tc_config_id = %d" tc_config_id) in
        let tc_config_info = Sql.exec_exn ~conn ~query in
        let parts = List.map ~f:(string_of_config_info_part tc_config_info)
          (List.range 1 tc_config_info#nfields) in
        sprintf "\"%d\":{%s}" tc_config_id (concat parts)
      in
      let string_of_som_config som_config_id =
        let query =
          (sprintf "SELECT * FROM som_config_%d " som_id) ^
          (sprintf "WHERE som_config_id = %d" som_config_id) in
        let som_config_info = Sql.exec_exn ~conn ~query in
        let parts = List.map ~f:(string_of_config_info_part som_config_info)
          (List.range 1 som_config_info#nfields) in
        sprintf "\"%d\":{%s}" som_config_id (concat parts)
      in
      let tc_configs = concat (List.map ~f:string_of_tc_config tc_config_ids) in
      let som_configs =
        concat (List.map ~f:string_of_som_config som_config_ids) in
      let string_of_split_by split_by =
        sprintf "\"%s\":\"%s\"" split_by.(0) split_by.(1) in
      let split_bys_str =
        concat_array (Array.map ~f:string_of_split_by split_bys#get_all) in
      (* Convert everything into a JSON string. *)
      "{" ^
      (sprintf "\"tc_fqn\": \"%s\"," tc_fqn) ^
      (sprintf "\"tc_desc\": \"%s\"," tc_desc) ^
      (sprintf "\"tc_configs\": {%s}," tc_configs) ^
      (sprintf "\"som_id\": %d," som_id) ^
      (sprintf "\"som_name\": \"%s\"," som_name) ^
      (sprintf "\"som_polarity\": \"%s\"," more_is_better) ^
      (sprintf "\"som_units\": \"%s\"," units) ^
      (sprintf "\"som_configs\": {%s}," som_configs) ^
      (sprintf "\"split_bys\": {%s}" split_bys_str) ^
      "}"
    in
    let report_plots_str =
      let arr = Array.map ~f:string_of_report_plot report_plots#get_all in
      concat (Array.to_list arr) in
    printf "{";
    printf "\"id\": %d," report_id;
    printf "\"desc\": \"%s\"," desc;
    printf "\"xaxis\": \"%s\"," xaxis;
    printf "\"yaxis\": \"%s\"," yaxis;
    printf "\"builds\": {";
    printf "\"primary\": [%s]," (string_of_builds primary_builds);
    printf "\"secondary\": [%s]" (string_of_builds secondary_builds);
    printf "},";
    printf "\"plots\": [%s]" report_plots_str;
    printf "}"
end
