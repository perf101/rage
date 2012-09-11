open Core.Std
open Utils

let t ~args = object (self)
  inherit Html_handler.t ~args

  method private write_body =
    printf "<h2>Report Generator</h2>\n";
    printf "<form action='/' method='post'>\n";
    printf "<input type='hidden' name='p' value='report_create' />\n";
    (* Show input boxes for entering basic information. *)
    printf "<hr /><h3>Basic information</h3>\n";
    printf "Report description: <input type='text' name='desc' /><br />\n";
    (* X/Y axis choice. *)
    printf "<hr /><h3>Axes</h3>\n";
    print_empty_x_axis_choice ~conn;
    print_empty_y_axis_choice ~conn;
    (* Display standard and all builds in dropboxes. *)
    printf "<hr /><h3>Builds to compare</h3>\n";
    let query = "SELECT build_name, build_number FROM standard_builds " ^
      "ORDER BY build_name" in
    let result = Sql.exec_exn ~conn ~query in
    let standard_build_names = Sql.get_col ~result ~col:0 in
    let standard_build_numbers = Sql.get_col ~result ~col:1 in
    let standard_builds =
      List.combine_exn standard_build_names standard_build_numbers in
    let query =
      "SELECT DISTINCT build_number FROM builds ORDER BY build_number" in
    let all_builds = Sql.get_col ~result:(Sql.exec_exn ~conn ~query) ~col:0 in
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
    let test_cases = Sql.exec_exn ~conn ~query in
    let process_tc tc_fqn tc_desc =
      printf "<h4 class='clear'>%s (<i>%s</i>)</h4>\n" tc_fqn tc_desc;
      let tc_tbl = sprintf "tc_config_%s" tc_fqn in
      print_options_for_fields conn tc_tbl ("tc-" ^ tc_fqn);
      let query = "SELECT som_id, som_name FROM soms " ^
        (sprintf "WHERE tc_fqn='%s'" tc_fqn) ^
        "ORDER BY som_id" in
      let soms = Sql.exec_exn ~conn ~query in
      let process_som som_id som_name =
        printf "<input name='include_som_%s' type='checkbox' />" som_id;
        printf "%s (<i>%s</i>)" som_id som_name;
        printf "<span id='view_container_%s' style='display: none'>[" som_id;
        printf "<a id='view_%s' target='_blank'>View</a>" som_id;
        printf "]</span>";
        printf "<span id='configs_%s' style='display: none'></span>" som_id;
        printf "<span id='points_%s' style='display: none'></span>" som_id;
        printf "<span id='tc_of_%s' style='display: none'>%s</span>" som_id tc_fqn;
        printf "<br class='clear' />";
        match som_config_tbl_exists ~conn (int_of_string som_id) with
        | som_config_tbl, true ->
            print_options_for_fields conn som_config_tbl ("som-" ^ som_id)
        | _ -> ()
      in
      Array.iter ~f:(fun r -> process_som r.(0) r.(1)) soms#get_all
    in
    Array.iter ~f:(fun r -> process_tc r.(0) r.(1)) test_cases#get_all;
    printf "<hr /><input type='submit' value='Create Report' />\n";
    printf "</form>\n";
    self#include_javascript
end
