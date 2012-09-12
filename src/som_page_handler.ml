open Core.Std
open Utils

let t ~args = object (self)
  inherit Html_handler.t ~args

  val show_for_value = "0"

  method private write_som_info som_info =
    let i = som_info#get_all.(0) in
    let name = sprintf "<span class='som_name'>%s</span>" i.(1) in
    let prefix = "<div class='som'>SOM:" in
    let suffix = "</div><br />\n" in
    printf "%s %s (id: %s, tc: %s) %s" prefix name i.(0) i.(2) suffix

  method private write_legend_position_choice id =
    printf "<div id='%s'>\n" id;
    let label = "Legend Position" in
    let legend_positions = [
      "North-East", "ne";
      "North-West", "nw";
      "South-East", "se";
      "South-West", "sw";
      "(nowhere)", "__";
    ] in
    print_select ~label ~attrs:[("name", id)] legend_positions;
    printf "</div>\n"

  method private write_symbol_choice id =
    printf "<div id='%s'>\n" id;
    let label = "Symbol to use" in
    let symbols = ["Diamond"; "Circle"; "Cross"; "Square"; "Triangle"] in
    print_select_list ~label ~attrs:[("name", id)] ~selected:["Circle"] symbols;
    printf "</div>\n"

  method private write_filter_table job_ids builds job_attributes configs
      som_configs_opt machines =
    (* LABELS *)
    let som_config_labels =
      match som_configs_opt with None -> [] | Some som_configs ->
      List.tl_exn (som_configs#get_fnames_lst) in
    let labels = ["job_id"; "product"; "branch"; "build_number"; "build_tag";
      "dom0_memory_static_max"; "dom0_memory_target"; "cc_restrictions"] @
      machines#get_fnames_lst @ configs#get_fnames_lst @ som_config_labels in
    (* OPTIONS *)
    let job_id_lst = get_options_for_field job_ids 0 in
    let branch_lst = get_options_for_field builds 0 in
    let build_no_lst = get_options_for_field builds 1 in
    let tag_lst = get_options_for_field builds 2 in
    let product_lst = get_options_for_field builds 3 in
    let dom0_memory_static_max_lst = get_options_for_field job_attributes 0 in
    let dom0_memory_target_lst = get_options_for_field job_attributes 1 in
    let cc_restrictions_lst = get_options_for_field job_attributes 2 in
    let machine_options_lst = List.map (List.range 0 machines#nfields)
      ~f:(fun col -> get_options_for_field machines col) in
    let config_options_lst = List.map (List.range 0 configs#nfields)
      ~f:(fun col -> get_options_for_field configs col) in
    let som_config_options_lst =
      match som_configs_opt with None -> [] | Some som_configs ->
      List.map (List.range 1 som_configs#nfields)
        ~f:(fun col -> get_options_for_field som_configs col) in
    let options_lst = [job_id_lst; product_lst; branch_lst; build_no_lst;
      tag_lst; dom0_memory_static_max_lst; dom0_memory_target_lst;
      cc_restrictions_lst] @ machine_options_lst @ config_options_lst @
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

  method private write_body =
    let som_id = int_of_string (List.Assoc.find_exn params "som") in
    let _, tc_config_tbl = get_tc_config_tbl_name conn som_id in
    let query =
      sprintf "SELECT * FROM soms WHERE som_id=%d" som_id in
    let som_info = Sql.exec_exn ~conn ~query in
    let query = "SELECT * FROM " ^ tc_config_tbl in
    let configs = Sql.exec_exn ~conn ~query in
    let query = "SELECT DISTINCT job_id FROM measurements WHERE " ^
      (sprintf "som_id=%d" som_id) in
    let job_ids = Sql.exec_exn ~conn ~query in
    let query =
      "SELECT DISTINCT branch, build_number, build_tag, product " ^
      "FROM builds AS b, jobs AS j, measurements AS m " ^
      "WHERE m.job_id=j.job_id AND j.build_id=b.build_id " ^
      (sprintf "AND m.som_id=%d" som_id) in
    let builds = Sql.exec_exn ~conn ~query in
    let query = "SELECT DISTINCT " ^
      "dom0_memory_static_max, dom0_memory_target, cc_restrictions " ^
      "FROM tc_config AS c, jobs AS j, measurements AS m " ^
      "WHERE m.job_id=j.job_id AND j.job_id=c.job_id " ^
      (sprintf "AND m.som_id=%d" som_id) in
    let job_attributes = Sql.exec_exn ~conn ~query in
    let som_config_tbl, som_tbl_exists = som_config_tbl_exists ~conn som_id in
    let som_configs_opt =
      if not som_tbl_exists then None else
      let query = sprintf "SELECT * FROM %s" som_config_tbl in
      Some (Sql.exec_exn ~conn ~query) in
    let query =
      "SELECT DISTINCT machine_name, machine_type, cpu_model, number_of_cpus " ^
      "FROM machines AS mn, tc_config AS c, measurements AS mr " ^
      "WHERE mn.machine_id=c.machine_id AND c.job_id=mr.job_id " ^
      (sprintf "AND som_id=%d" som_id) in
    let machines = Sql.exec_exn ~conn ~query in
    self#write_som_info som_info;
    print_select_list ~label:"View" ~attrs:[("id", "view")] ["Graph"; "Table"];
    printf "<form name='optionsForm'>\n";
    print_x_axis_choice ~conn configs som_configs_opt;
    print_y_axis_choice ~conn configs som_configs_opt;
    let checkbox name caption =
      printf "<div id='%s' style='display: inline'>\n" name;
      printf "<input type='checkbox' name='%s' />%s\n" name caption;
      printf "</div>\n" in
    checkbox "show_points" "Show points";
    checkbox "show_avgs" "Show averages";
    checkbox "show_dist" "Show distributions";
    checkbox "x_from_zero" "Force X from 0";
    checkbox "y_fromto_zero" "Force Y from/to 0";
    checkbox "x_as_seq" "Force X data as sequence";
    checkbox "y_as_seq" "Force Y data as sequence";
    checkbox "show_all_meta" "Show all meta-data";
    checkbox "xaxis_log" "Log scale X";
    checkbox "yaxis_log" "Log scale Y";
    printf "<br />\n";
    self#write_legend_position_choice "legend_position";
    self#write_symbol_choice "symbol";
    printf "<br />\n";
    self#write_filter_table job_ids builds job_attributes configs
      som_configs_opt machines;
    printf "</form>\n";
    let submit_prefix = "<input type='submit' id=" in
    printf "%s'reset_config' value='Reset Configuration' />" submit_prefix;
    printf "%s'get_img' value='Get Image' />" submit_prefix;
    printf "%s'get_tinyurl' value='Get Tiny URL' />" submit_prefix;
    printf "<a id='tinyurl' style='display: none' title='Tiny URL'></a>";
    printf "%s'stop_plotting' value='Stop Plotting' />" submit_prefix;
    printf "<br /><img id='progress_img' src='progress.gif' />\n";
    printf "<div class='graph_container'>";
    printf "<div class='yaxis'></div>";
    printf "<div id='graph' style='width: 1000px; height: 600px' class='graph'></div>";
    printf "<div class='xaxis'></div>";
    printf "</div>";
    printf "<div id='table'></div>";
    self#include_javascript;
end
