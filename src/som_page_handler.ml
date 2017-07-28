open! Core.Std
open Utils

let jira_hostname = "jira.uk.xensource.com"

let t ~args = object (self)
  inherit Html_handler.t ~args

  val show_for_value = "0"

  method private write_som_info som_info =
    let i = som_info#get_all.(0) in
    let id = i.(0) in
    let name = sprintf "<span class='som_name'>%s</span>" i.(1) in
    let jira_link = sprintf "<a href='http://%s/browse/SOM-%s'>SOM-%s</a>" jira_hostname id id in
    let prefix = "<div class='som'>" in
    let suffix = "</div><br />\n" in
    printf "%s%s: %s (tc: %s) %s" prefix jira_link name i.(2) suffix

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

  method private write_filter_table job_ids builds job_attributes config_columns tc_config_tbl
      som_configs_opt machines =
    let config_column_names = config_columns#get_fnames_lst in

    (* LABELS *)
    let som_config_labels =
      match som_configs_opt with None -> [] | Some som_configs -> som_configs#get_fnames_lst in
    let labels =
      Utils.job_fields @
      Utils.build_fields @
      Utils.tc_config_fields @
      machines#get_fnames_lst @ config_column_names @ som_config_labels in

    (* OPTIONS *)
    let options_lst_of_dbresult dbresult =
      let data = dbresult#get_all in
      List.map (List.range 0 dbresult#nfields)
        ~f:(fun col -> get_options_for_field dbresult ~data col) in

    let job_lsts = List.map Utils.job_fields ~f:(get_options_for_field_once_byname job_ids) in
    let build_lsts = List.map Utils.build_fields ~f:(get_options_for_field_once_byname builds) in
    let job_attrs_lsts = List.mapi ~f:(fun i job_attr -> get_options_for_field_once job_attributes i) Utils.tc_config_fields in

    let machine_options_lst = options_lst_of_dbresult machines in

    let config_options_lst = List.map config_column_names ~f:(fun config_name ->
      let query = sprintf "SELECT DISTINCT %s FROM %s ORDER BY %s" config_name tc_config_tbl config_name in
      let configs = Sql.exec_exn ~conn ~query in
      get_options_for_field_once configs 0
    ) in

    let som_config_options_lst =
      match som_configs_opt with None -> [] | Some som_configs ->
        options_lst_of_dbresult som_configs in

    let options_lst =
      job_lsts @
      build_lsts @
      job_attrs_lsts @ machine_options_lst @ config_options_lst @
      som_config_options_lst in
    let print_table_for (label, options) =
      printf "<table border='1' class='filter_table'>\n";
      printf "<tr><th name='title_%s%s'>%s</th></tr>\n" values_prefix label label;
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
    List.iter ~f:print_table_for (List.zip_exn labels options_lst)

  method private write_body =
    let som_id = int_of_string (List.Assoc.find_exn params "som") in
    let _, tc_config_tbl = get_tc_config_tbl_name conn som_id in
    let query =
      sprintf "SELECT * FROM soms WHERE som_id=%d" som_id in
    let som_info = Sql.exec_exn ~conn ~query in
    let query = "SELECT * FROM " ^ tc_config_tbl ^ " LIMIT 0" in
    let config_columns = Sql.exec_exn ~conn ~query in
    let job_fields = String.concat ~sep:", " Utils.job_fields in
    let query = "SELECT DISTINCT " ^ job_fields ^ " FROM soms_jobs WHERE " ^
      (sprintf "som_id=%d" som_id) in
    let job_ids = Sql.exec_exn ~conn ~query in
    let build_fields = String.concat ~sep:", " Utils.build_fields in
    let query =
      "SELECT DISTINCT " ^ build_fields ^ " " ^
      (sprintf "FROM builds AS b, jobs AS j, (select distinct job_id from soms_jobs where som_id=%d) AS m " som_id) ^
      "WHERE m.job_id=j.job_id AND j.build_id=b.build_id "
    in
    let builds = Sql.exec_exn ~conn ~query in
    let query = "SELECT DISTINCT " ^ (String.concat ~sep:", " Utils.tc_config_fields) ^ " " ^
      (sprintf "FROM tc_config AS c, jobs AS j, (select distinct job_id from soms_jobs where som_id=%d) AS m " som_id) ^
      "WHERE m.job_id=j.job_id AND j.job_id=c.job_id "
    in
    let job_attributes = Sql.exec_exn ~conn ~query in
    let som_config_tbl, som_tbl_exists = som_config_tbl_exists ~conn som_id in
    let som_configs_opt =
      if not som_tbl_exists then None else
      let query = sprintf "SELECT * FROM %s" som_config_tbl in
      Some (Sql.exec_exn ~conn ~query) in
    let query =
      "SELECT DISTINCT machine_name, machine_type, cpu_model, number_of_cpus " ^
      (sprintf "FROM machines AS mn, tc_config AS c, (select distinct job_id from soms_jobs where som_id=%d) AS mr " som_id) ^
      "WHERE mn.machine_id=c.machine_id AND c.job_id=mr.job_id "
    in
    let machines = Sql.exec_exn ~conn ~query in
    printf "<table width=\"100%%\" border=\"0\">\n<tr><td>\n";
    self#write_som_info som_info;
    print_select_list ~label:"View" ~attrs:[("id", "view")] ["Graph"; "Table"];
    printf "<form name='optionsForm'>\n";
    print_x_axis_choice ~conn config_columns som_configs_opt;
    print_y_axis_choice ~conn config_columns som_configs_opt;
    let checkbox name caption =
      printf "<div id='%s' style='display: inline'>\n" name;
      printf "<input type='checkbox' name='%s' />%s\n" name caption;
      printf "</div>\n" in
    printf "</td><td>\n";
    checkbox "show_points" "Show points";
    checkbox "show_avgs" "Show averages";
    checkbox "show_dist" "Show distributions";
    printf "<br/>\n";
    checkbox "x_from_zero" "Force X from 0";
    checkbox "y_fromto_zero" "Force Y from/to 0";
    printf "<br/>\n";
    checkbox "x_as_seq" "Force X data as sequence";
    checkbox "y_as_seq" "Force Y data as sequence";
    printf "<br/>\n";
    checkbox "x_as_num" "Force X data as numeric";
    printf "<br/>\n";
    checkbox "show_all_meta" "Show all meta-data";
    printf "<br/>\n";
    checkbox "xaxis_log" "Log scale X";
    checkbox "yaxis_log" "Log scale Y";
    printf "<br/>\n";
    checkbox "auto_redraw" "Auto Redraw";
    printf "<br />\n";
    self#write_legend_position_choice "legend_position";
    self#write_symbol_choice "symbol";
    printf "</td></tr>\n</table>\n";
    self#write_filter_table job_ids builds job_attributes config_columns tc_config_tbl
      som_configs_opt machines;
    printf "</form>\n";
    printf "<div id='graph_title'></div>\n";
    let submit_prefix = "<input type='submit' id=" in
    printf "%s'reset_config' value='Reset Configuration' />" submit_prefix;
    printf "%s'get_img' value='Get Image' />" submit_prefix;
    printf "%s'get_tinyurl' value='Get Tiny URL' />" submit_prefix;
    printf "<a id='tinyurl' style='display: none' title='Tiny URL'></a>";
    printf "%s'stop_plotting' value='Stop Plotting' />" submit_prefix;
    printf "%s'redraw' value='Redraw' />" submit_prefix;
    printf "<br /><img id='progress_img' src='progress.gif' />\n";
    printf "<div class='graph_container'>";
    printf "<div class='yaxis'></div>";
    printf "<div id='graph' style='width: 1000px; height: 600px' class='graph'></div>";
    printf "<div class='xaxis'></div>";
    printf "</div>";
    printf "<div id='table'></div>";
    self#include_javascript;
end
