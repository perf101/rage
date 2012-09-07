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
  | AxesAsync
  | Default
  | ReportCreate of (string * string) list
  | ReportGenerator
  | Reports
  | Report of int
  | ReportAsync of int
  | ReportClone of int
  | ReportDelete of int
  | Som of int * int list
  | SomAsync of int * (string * string) list
  | Soms
  | SomsAsync
  | SomsByTc
  | CreateTiny of string
  | RedirectTiny of int

let string_of_int_list is =
  concat (List.map ~f:string_of_int is)

let string_of_som_place name id cids =
  (sprintf "%s: %d" name id) ^ (if cids = [] then "" else
   sprintf " (Config IDs: %s)" (string_of_int_list cids))

let string_of_place = function
  | AxesAsync -> "AxesAsync"
  | Default -> "Default"
  | ReportCreate _ -> "ReportCreate"
  | ReportGenerator -> "ReportGenerator"
  | Reports -> "Reports"
  | Report id -> sprintf "Report %d" id
  | ReportAsync id -> sprintf "ReportAsync %d" id
  | ReportClone id -> sprintf "ReportClone %d" id
  | ReportDelete id -> sprintf "ReportDelete %d" id
  | Som (id, cids) -> string_of_som_place "Som" id cids
  | Soms -> "SOMs"
  | SomsAsync -> "SOMs Async"
  | SomsByTc -> "SOMs by Test Case"
  | SomAsync (id, _) -> string_of_som_place "SomAsync" id []
  | CreateTiny url -> sprintf "CreateTiny %s" url
  | RedirectTiny id -> sprintf "RedirectTiny %d" id

let path =
  let exe = Sys.argv.(0) in
  String.sub exe ~pos:0 ~len:((String.rindex_exn exe '/') + 1)

let get_request () = Sys.getenv_exn "QUERY_STRING"

let header_inserted : bool ref = ref false

let insert_header () =
  printf "Content-type: text/html\n\n";
  cat (path ^ "header.html");
  header_inserted := true

let insert_footer () = cat (path ^ "footer.html")

let failwith msg =
  if not !header_inserted then insert_header ();
  failwith msg

let params_of_request get_req =
  let post_req = In_channel.input_all In_channel.stdin in
  let req = get_req ^ (if post_req = "" then "" else "&" ^ post_req) in
  let parts = String.split req ~on:'&' in
  let opt_split part =
    Option.value ~default:(part, "") (String.lsplit2 part ~on:'=') in
  let remove_brackets (n, v) =
    Option.value ~default:n (String.chop_suffix n ~suffix:"%5B%5D"), v in
  List.stable_dedup (List.map ~f:(Fn.compose remove_brackets opt_split) parts)

let string_of_params params =
  concat ~sep:"\n" (List.map ~f:(fun (k, v) -> k ^ " => " ^ v) params)

let values_for_key params key =
  List.fold params ~init:[]
    ~f:(fun acc (k, v) -> if k = key then v::acc else acc)

let get_first_val params k d =
  Option.value ~default:d (List.hd (values_for_key params k))

let place_of_request req =
  let open List.Assoc in
  let params = params_of_request req in
  match find params "axesasync" with Some _ -> AxesAsync | None ->
  match find params "soms" with Some _ -> Soms | None ->
  match find params "somsasync" with Some _ -> SomsAsync | None ->
  match find params "somsbytc" with Some _ -> SomsByTc | None ->
  match find params "report_generator" with Some _ -> ReportGenerator | None ->
  match find params "report_create" with Some _ -> ReportCreate params | None ->
  match find params "report_clone" with
  | Some _ ->
    ReportClone (int_of_string (List.hd_exn (values_for_key params "id")))
  | None ->
  match find params "report_delete" with
  | Some _ ->
    ReportDelete (int_of_string (List.hd_exn (values_for_key params "id")))
  | None ->
  match find params "reports" with Some _ -> Reports | None ->
  match find params "report" with
  | Some id_str ->
      let id = int_of_string id_str in
      if Option.is_some (find params "async") then ReportAsync id else Report id
  | None ->
  match find params "som" with
  | None ->
    begin match find params "t" with
    | Some id -> RedirectTiny (int_of_string id)
    | None ->
      begin match find params "action" with
      | Some "CreateTiny" -> CreateTiny (find_exn params "url")
      | _ -> Default
      end
    end
  | Some id_s ->
      let id = int_of_string id_s in
      let cids_s = values_for_key params "config_id" in
      let cids = List.map ~f:int_of_string cids_s in
      let cids_sorted = List.sort ~cmp:compare cids in
      match find params "async" with
      | Some "true" -> SomAsync (id, params)
      | _ -> Som (id, cids_sorted)

let get_place () = place_of_request (get_request ())

let som_config_tbl_exists conn som_id =
  let som_config_tbl = sprintf "som_config_%d" som_id in
  som_config_tbl, Sql.tbl_exists ~conn ~tbl:som_config_tbl

let get_std_xy_choices conn =
  let machine_field_lst =
    List.tl_exn (Sql.get_col_names ~conn ~tbl:"machines") in
  "branch" :: "build_number" :: "build_tag" ::
  "dom0_memory_static_max" :: "dom0_memory_target" ::
  "cc_restrictions" ::
  machine_field_lst

let get_std_x_choices = get_std_xy_choices
let get_std_y_choices conn = "result" :: get_std_xy_choices conn

let get_xy_choices conn configs som_configs_opt =
  let som_configs_lst = match som_configs_opt with
    | None -> []
    | Some som_configs -> List.tl_exn som_configs#get_fnames_lst
  in get_std_xy_choices conn @ configs#get_fnames_lst @ som_configs_lst

let print_axis_choice label id choices =
  printf "<div id='%s' style='display: inline'>\n" id;
  print_select_list ~label ~attrs:[("name", id)] choices;
  printf "</div>\n"

let print_empty_x_axis_choice conn =
  print_axis_choice "X axis" "xaxis" []

let print_empty_y_axis_choice conn =
  print_axis_choice "Y axis" "yaxis" []

let print_x_axis_choice conn configs som_configs_opt =
  print_axis_choice "X axis" "xaxis"
    (get_xy_choices conn configs som_configs_opt)

let print_y_axis_choice conn configs som_configs_opt =
  print_axis_choice "Y axis" "yaxis"
    ("result" :: (get_xy_choices conn configs som_configs_opt))

let report_generator_handler ~conn =
  insert_header ();
  printf "<h2>Report Generator</h2>\n";
  printf "<form action='/' method='post'>\n";
  printf "<input type='hidden' name='report_create' />\n";
  (* Show input boxes for entering basic information. *)
  printf "<hr /><h3>Basic information</h3>\n";
  printf "Report description: <input type='text' name='desc' /><br />\n";
  (* X/Y axis choice. *)
  printf "<hr /><h3>Axes</h3>\n";
  print_empty_x_axis_choice conn;
  print_empty_y_axis_choice conn;
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
      match som_config_tbl_exists conn (int_of_string som_id) with
      | som_config_tbl, true ->
          print_options_for_fields conn som_config_tbl ("som-" ^ som_id)
      | _ -> ()
    in
    Array.iter ~f:(fun r -> process_som r.(0) r.(1)) soms#get_all
  in
  Array.iter ~f:(fun r -> process_tc r.(0) r.(1)) test_cases#get_all;
  printf "<hr /><input type='submit' value='Create Report' />\n";
  printf "</form>\n";
  printf "<script src='rage.js'></script>";
  insert_footer ()

let get_builds_from_params conn params key tbl =
  let build_vals = List.filter ~f:((<>) "NONE") (values_for_key params key) in
  let builds_str =
    if List.mem "ALL" ~set:build_vals
    then
      let result =
        Sql.exec_exn ~conn ~query:("SELECT build_number FROM " ^ tbl) in
      Sql.get_col ~result ~col:0
    else build_vals
  in List.map ~f:int_of_string builds_str

let get_build_group conn params group =
  let std_builds = get_builds_from_params conn params
    (group ^ "_standard_builds") "standard_builds" in
  let all_builds = get_builds_from_params conn params
    (group ^ "_all_builds") "builds" in
  List.sort ~cmp:compare (List.dedup (std_builds @ all_builds))

let javascript_redirect url =
  printf "Content-type: text/html\n\n";
  printf "<html><head>\n";
  printf "<script language='javascript' type='text/javascript'>\n";
  printf "window.location.replace(decodeURIComponent('%s'));\n" url;
  printf "</script>\n</head><body></body></html>\n"

let report_clone_handler ~conn id =
  (* Define a couple of helper functions for cloning rows. *)
  let field_to_tuple ?(origin = false) ~result ~key ?next ~row col v =
    let k = result#fname col in
    let v = match k with
    | k when k = key -> Option.value_map next ~f:string_of_int ~default:v
    | "report_desc" -> v ^ " COPY"
    | _ -> match v with "t" -> "true" | "f" -> "false" | _ -> v
    in
    if k = key && origin || result#getisnull row col then None else Some (k, v)
  in
  let clone_rows ~tbl ~key ~prev ~next =
    let query = sprintf "SELECT * FROM %s WHERE %s = %d" tbl key prev in
    let result = Sql.exec_exn ~conn ~query in
    List.iteri ~f:(fun i row ->
      let tuples = List.filter_mapi row
        ~f:(field_to_tuple ~result ~key ~next ~row:i) in
      Sql.ensure_inserted ~conn ~tbl ~tuples
    ) result#get_all_lst;
  in
  (* Get definition of report to clone. *)
  let query = sprintf "SELECT * FROM reports WHERE report_id = %d" id in
  let result = Sql.exec_exn ~conn ~query in
  let source = result#get_tuple_lst 0 in
  (* Duplicate its entry in "reports" table. *)
  let tuples = List.filter_mapi source
    ~f:(field_to_tuple ~origin:true ~result ~key:"report_id" ~row:0) in
  let id' = Sql.ensure_inserted_get_id ~conn ~tbl:"reports" ~tuples in
  (* Clone corresponding rows in "report_builds". *)
  clone_rows ~tbl:"report_builds" ~key:"report_id" ~prev:id ~next:id';
  (* Find all corresponding plots. *)
  let query = "SELECT plot_id, report_id, graph_number, som_id " ^
    sprintf "FROM report_plots WHERE report_id = %d" id in
  let result = Sql.exec_exn ~conn ~query in
  (* For each plot.. *)
  let process_plot plot =
    (* Get its current ID. *)
    let plot_id = int_of_string (List.hd_exn plot) in
    (* Create a copy of its definition, and get the copy's ID. *)
    let tuples = List.filter_mapi plot
      ~f:(field_to_tuple ~result ~key:"report_id" ~next:id' ~row:0) in
    let tuples = List.filter ~f:(fun (k, _) -> k <> "plot_id") tuples in
    let plot_id' = Sql.ensure_inserted_get_id ~conn ~tbl:"report_plots" ~tuples in
    (* Clone corresponding rows for this plot, using the copy's ID. *)
    let tbls = List.map ~f:((^) "report_plot_")
      ["tc_configs"; "som_configs"; "split_bys"] in
    List.iter tbls ~f:(fun tbl ->
      clone_rows ~tbl ~key:"plot_id" ~prev:plot_id ~next:plot_id')
  in
  List.iter ~f:process_plot result#get_all_lst;
  (* Redirect back to reports' page. *)
  javascript_redirect "/?reports"

let report_delete_handler ~conn id =
  let query = sprintf "DELETE FROM reports WHERE report_id = %d" id in
  Sql.exec_ign_exn ~conn ~query;
  javascript_redirect "/?reports"

(** Given [["a"; "b"]; ["1"; "2"]; ["x"; "y"]], this function outputs
 * [ ["a"; "1"; "x"]; ["a"; "1"; "y"]; ["a"; "2"; "x"]; ["a"; "2"; "y"];
 *   ["b"; "1"; "x"]; ["b"; "1"; "y"]; ["b"; "2"; "x"]; ["b"; "2"; "y"] ]. *)
let generate_permutations dimensions =
  let rec explode base = function
    | [] -> [List.rev base]
    | d::ds -> List.concat (List.map ~f:(fun v -> explode (v::base) ds) d)
  in explode [] dimensions

let report_create_handler ~conn params =
  (* If "id" is specified, then modify report <id>. *)
  let id_opt = List.hd (values_for_key params "id") in
  ignore (Option.map id_opt ~f:(fun id ->
    let query = "DELETE FROM reports WHERE report_id = " ^ id in
    Sql.exec_ign_exn ~conn ~query
  ));
  (* Obtain metadata. *)
  let desc = List.hd_exn (values_for_key params "desc") in
  let xaxis = List.hd_exn (values_for_key params "xaxis") in
  let yaxis = List.hd_exn (values_for_key params "yaxis") in
  let tuples =
    (match id_opt with None -> [] | Some id -> [("report_id", id)]) @
    [("report_desc", desc); ("xaxis", xaxis); ("yaxis", yaxis)]
  in
  let report_id = Sql.ensure_inserted_get_id ~conn ~tbl:"reports" ~tuples in
  (* Gather and record builds. *)
  let primary_builds = get_build_group conn params "primary" in
  let secondary_builds = get_build_group conn params "secondary" in
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
    let split_by_graphs_values =
      List.map ~f:(values_for_key split_by_graph_params) split_by_graph_keys in
    let split_by_graphs_perms = generate_permutations split_by_graphs_values in
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
      match som_config_tbl_exists conn som_id with
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


  (* Print all GET parameters. *)
  (* List.iter ~f:(fun (k, v) -> printf "%s ===> %s<br />\n" k v) params; *)
  (* insert_footer () *)
  javascript_redirect "/?reports"

let reports_handler ~conn =
  insert_header ();
  let query = "SELECT report_id, report_desc FROM reports" in
  let result = Sql.exec_exn ~conn ~query in
  let print_report report =
    let id = report.(0) in
    let desc = report.(1) in
    printf "<tr>";
    printf "<td>%s</td>" id;
    printf "<td class='encoded'>%s</td>" desc;
    printf "<td><a href='/?report=%s'>View</a></td>" id;
    printf "<td><a href='/?report_generator&id=%s'>Edit</a></td>" id;
    printf "<td><a href='/?report_clone&id=%s'>Clone</a></td>" id;
    printf "<td><a href='/?report_delete&id=%s'>Delete</a></td>" id;
    printf "</tr>";
  in
  printf "<table border='1'>\n";
  printf "<tr><th>ID</th><th>Description</th><th colspan='4'>Actions</th></tr>";
  Array.iter ~f:print_report result#get_all;
  printf "</table>\n";
  printf "<a href='/?report_generator'>New</a>\n";
  printf "<script src='rage.js'></script>";
  insert_footer ()

let axes_async_handler ~conn =
  let std_x_axes = get_std_x_choices conn in
  let std_y_axes = get_std_y_choices conn in
  let string_of_axes choices =
    let quoted = List.map ~f:(fun c -> "\"" ^ c ^ "\"") choices in
    sprintf "[%s]" (String.concat ~sep:"," quoted)
  in
  printf "Content-type: application/json\n\n";
  printf "{";
  printf "\"std_x_axes\": %s," (string_of_axes std_x_axes);
  printf "\"std_y_axes\": %s" (string_of_axes std_y_axes);
  printf "}"

let report_async_handler ~conn report_id =
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
  printf "Content-type: application/json\n\n";
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

let js_only_handler () =
  insert_header ();
  printf "<script src='rage.js'></script>";
  insert_footer ()

let default_handler ~conn =
  insert_header ();
  printf "<ul>\n";
  printf "<li><a href='?reports'>Reports</a></li>\n";
  printf "<li><a href='?soms'>Scales of Measure</a></li>\n";
  printf "<li><a href='?somsbytc'>Scales of Measure by Test Case</a></li>\n";
  printf "</ul>\n";
  insert_footer ()

let soms_handler ~conn =
  insert_header ();
  let query = "SELECT som_id, som_name FROM soms " ^
              "ORDER BY som_id" in
  let result = Sql.exec_exn ~conn ~query in
  let print_row row_i tag row =
    match row_i with -1 -> print_row_header row | _ ->
    print_string "   <tr>";
    let som_id = List.nth_exn row 0 in
    let name = List.nth_exn row 1 in
    printf "<%s>%s</%s>" tag som_id tag;
    printf "<%s><a href='?som=%s'>%s</a></%s>" tag som_id name tag;
    print_string "   </tr>" in
  print_table_custom_row print_row result;
  insert_footer ()

let soms_async_handler ~conn =
  let query = "SELECT tc_fqn,description FROM test_cases ORDER BY tc_fqn" in
  let tcs = Sql.exec_exn ~conn ~query in
  let json_of_tc tc =
    sprintf "\"%s\":{\"desc\":\"%s\"}" tc.(0) tc.(1) in
  let tcs_json = concat_array (Array.map ~f:json_of_tc tcs#get_all) in
  let query = "SELECT som_id,som_name,tc_fqn FROM soms ORDER BY som_id" in
  let soms = Sql.exec_exn ~conn ~query in
  let json_of_som som =
    sprintf "\"%s\":{\"name\":\"%s\",\"tc\":\"%s\"}" som.(0) som.(1) som.(2) in
  let soms_json = concat_array (Array.map ~f:json_of_som soms#get_all) in
  printf "Content-type: application/json\n\n";
  printf "{\"tcs\":{%s},\"soms\":{%s}}" tcs_json soms_json

let get_tc_config_tbl_name conn som_id =
  let query = "SELECT tc_fqn FROM soms " ^
              "WHERE som_id = " ^ (string_of_int som_id) in
  let result = Sql.exec_exn ~conn ~query in
  let tc_fqn = String.lowercase (result#getvalue 0 0) in
  (tc_fqn, "tc_config_" ^ tc_fqn)

let get_branch_ordering ~conn branches =
  let branches_uniq = List.dedup ~compare branches in
  let query =
    "SELECT branch FROM branch_order WHERE branch IN ('" ^
    (concat ~sep:"','" branches_uniq) ^ "') ORDER BY seq_number" in
  let data = Sql.exec_exn ~conn ~query in
  let process_row i row = (i+1, List.nth_exn row 0) in
  Some (List.mapi data#get_all_lst ~f:process_row)

let print_som_info som_info =
  let i = som_info#get_all.(0) in
  let name = sprintf "<span class='som_name'>%s</span>" i.(1) in
  let prefix = "<div class='som'>SOM:" in
  let suffix = "</div><br />\n" in
  printf "%s %s (id: %s, tc: %s) %s" prefix name i.(0) i.(2) suffix

let print_legend_position_choice id =
  printf "<div id='%s'>\n" id;
  let label = "Legend Position" in
  let legend_positions = [
    ("North-East", "ne");
    ("North-West", "nw");
    ("South-East", "se");
    ("South-West", "sw");
    ("(nowhere)", "__");
  ] in
  print_select ~label ~attrs:[("name", id)] legend_positions;
  printf "</div>\n"

let filter_prefix = "f_"
let values_prefix = "v_"
let show_for_value = "0"
let filter_by_value = "1"

let print_filter_table job_ids builds job_attributes configs som_configs_opt
    machines =
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

let show_configurations ~conn som_id tc_config_tbl =
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
  let som_config_tbl, som_tbl_exists = som_config_tbl_exists conn som_id in
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
  print_som_info som_info;
  print_select_list ~label:"View" ~attrs:[("id", "view")] ["Graph"; "Table"];
  printf "<form name='optionsForm'>\n";
  print_x_axis_choice conn configs som_configs_opt;
  print_y_axis_choice conn configs som_configs_opt;
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
  print_legend_position_choice "legend_position";
  printf "<br />\n";
  print_filter_table job_ids builds job_attributes configs som_configs_opt machines;
  printf "</form>\n";
  let submit_prefix = "<input type='submit' id=" in
  printf "%s'reset_config' value='Reset Configuration' />" submit_prefix;
  printf "%s'get_img' value='Get Image' />" submit_prefix;
  printf "%s'get_tinyurl' value='Get Tiny URL' />" submit_prefix;
  printf "<a id='tinyurl' style='display: none' title='Tiny URL'></a>";
  printf "%s'stop_plotting' value='Stop Plotting' />" submit_prefix;
  printf "<br /><img id='progress_img' src='progress.gif' />\n";
  printf "<div id='graph' style='width: 1000px; height: 600px'></div>";
  printf "<div id='table'></div>";
  printf "<script src='rage.js'></script>"

let som_handler ~place:_ ~conn som_id _ =
  insert_header ();
  let _, tc_config_tbl = get_tc_config_tbl_name conn som_id in
  show_configurations ~conn som_id tc_config_tbl;
  insert_footer ()

(** Pre-generated regexp for use within som_async_handler. *)
let quote_re = Str.regexp "\""

let should_sort_alphabetically col_types col_name force_as_seq =
  match force_as_seq with true -> true | false ->
  match String.Table.find col_types col_name with None -> false | Some ty ->
  Sql.Type.is_quoted ty

let get_generic_string_mapping rows col col_name col_types force_as_seq =
  if not (should_sort_alphabetically col_types col_name force_as_seq)
  then None else
  let col_data = Array.to_list (Array.map ~f:(fun row -> row.(col)) rows) in
  let uniques = List.dedup col_data in
  let sorted = List.sort ~cmp:compare uniques in
  Some (List.mapi sorted ~f:(fun i x -> (i+1, x)))

let strings_to_numbers rows col col_name col_types label force_as_seq =
  let mapping_opt =
    get_generic_string_mapping rows col col_name col_types force_as_seq in
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
    if String.is_prefix k ~prefix && (Option.is_none value || Some v = value)
    then String.chop_prefix k ~prefix else None
  )
let get_filter_params params = select_params params filter_prefix
let get_values_params params = select_params params values_prefix
let get_relevant_params params =
  get_filter_params params @ get_values_params params

let som_async_handler ~conn som_id params =
  printf "Content-type: application/json\n\n";
  let tc_fqn, tc_config_tbl = get_tc_config_tbl_name conn som_id in
  let som_config_tbl, som_tbl_exists = som_config_tbl_exists conn som_id in
  (* determine filter columns and their types *)
  let tbls = ["measurements"; "jobs"; "builds"; "tc_config"; "machines";
    tc_config_tbl] @
    (if som_tbl_exists then [som_config_tbl] else []) in
  let col_fqns = get_column_fqns_many conn tbls in
  let col_types = get_column_types_many conn tbls in
  let xaxis = get_first_val params "xaxis" "branch" in
  let yaxis = get_first_val params "yaxis" "result" in
  let compose_keys ~xaxis ~yaxis ~rest =
    let deduped = List.stable_dedup rest in
    let filter_cond = non (List.mem ~set:[xaxis; yaxis]) in
    xaxis :: yaxis :: (List.filter ~f:filter_cond deduped)
  in
  let keys =
    let rest = match get_first_val params "show_all_meta" "off" with
    | "on" -> String.Table.keys col_fqns
    | _ -> get_relevant_params params
    in compose_keys ~xaxis ~yaxis ~rest
  in
  let fqns = List.map keys ~f:(String.Table.find_exn col_fqns) in
  let filter = extract_filter col_fqns col_types params values_prefix in
  (* obtain SOM meta-data *)
  let query = sprintf "SELECT positive FROM soms WHERE som_id=%d" som_id in
  let metadata = Sql.exec_exn ~conn ~query in
  let positive = (Sql.get_first_entry_exn ~result:metadata) = "t" in
  (* obtain data from database *)
  let query =
    (sprintf "SELECT %s " (String.concat ~sep:", " fqns)) ^
    (sprintf "FROM %s " (String.concat ~sep:", " tbls)) ^
    (sprintf "WHERE measurements.tc_config_id=%s.tc_config_id "
             tc_config_tbl) ^
    (sprintf "AND measurements.som_id=%d " som_id) ^
    "AND measurements.job_id=jobs.job_id " ^
    "AND jobs.build_id=builds.build_id " ^
    "AND tc_config.job_id=jobs.job_id " ^
    (sprintf "AND tc_config.tc_fqn='%s' " tc_fqn) ^
    "AND tc_config.tc_config_id=measurements.tc_config_id " ^
    "AND tc_config.machine_id=machines.machine_id" ^
    (if som_tbl_exists
     then sprintf " AND measurements.som_config_id=%s.som_config_id"
          som_config_tbl else "") ^
    (if not (String.is_empty filter) then sprintf " AND %s" filter else "") in
  let data = Sql.exec_exn ~conn ~query in
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
  printf "\"positive\":%B," positive;
  printf "\"target\":\"%s\"," (get_first_val params "target" "graph");
  printf "\"part\":%s," (get_first_val params "part" "1");
  printf "\"xaxis\":\"%s\"," xaxis;
  printf "\"yaxis\":\"%s\"," yaxis;
  let x_as_seq = ("on" = get_first_val params "x_as_seq" "off") in
  let y_as_seq = ("on" = get_first_val params "y_as_seq" "off") in
  strings_to_numbers rows 0 xaxis col_types "x_labels" x_as_seq;
  strings_to_numbers rows 1 yaxis col_types "y_labels" y_as_seq;
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
    printf "{\"label\":\"%s\",\"color\":%d,\"data\":[" label i;
    print_concat data_lst;
    printf "]}"
  in
  List.iteri (ListKey.Table.to_alist all_series) ~f:process_series;
  printf "]}"

let createtiny_handler ~conn url =
  printf "Content-type: application/json\n\n";
  let tuples = [("url", url)] in
  let id = Sql.ensure_inserted_get_id ~conn ~tbl:"tiny_urls" ~tuples in
  printf "{\"id\":%d}" id

let print_404 () =
  printf "Status: 404 Not Found\n";
  printf "Content-Type: text/html\n\n";
  printf "<h1>404 --- this is not the page you are looking for ...</h1>"

let redirecttiny_handler ~conn id =
  let query = sprintf "SELECT url FROM tiny_urls WHERE key=%d" id in
  let result = Sql.exec_exn ~conn ~query in
  match result#ntuples with
  | 1 -> javascript_redirect (Sql.get_first_entry_exn ~result)
  | _ -> print_404 ()

let handle_request () =
  let start_time = Unix.gettimeofday () in
  let place = get_place () in
  let conn = new connection ~conninfo:Sys.argv.(1) () in
  begin match place with
    | AxesAsync -> axes_async_handler ~conn
    | Default -> default_handler ~conn
    | CreateTiny url -> createtiny_handler ~conn url
    | RedirectTiny id -> redirecttiny_handler ~conn id
    | ReportClone id -> report_clone_handler ~conn id
    | ReportDelete id -> report_delete_handler ~conn id
    | ReportCreate params -> report_create_handler ~conn params
    | ReportGenerator -> report_generator_handler ~conn
    | Reports -> reports_handler ~conn
    | Report _ -> js_only_handler ()
    | ReportAsync id -> report_async_handler ~conn id
    | Som (id, cids) -> som_handler ~place ~conn id cids
    | SomAsync (id, params) -> som_async_handler ~conn id params
    | Soms -> soms_handler ~conn
    | SomsAsync -> soms_async_handler ~conn
    | SomsByTc -> js_only_handler ()
  end;
  conn#finish;
  let elapsed_time = Unix.gettimeofday () -. start_time in
  debug (sprintf "==========> '%s': %fs." (string_of_place place) elapsed_time)

let bind_modules () =
  Sql.debug_fn := Some debug;
  Sql.show_sql := true;
  Sql.time_queries := true;
  Sql.ignore_limit_0 := true;
  Sql.mode := Sql.Live

let _ =
  bind_modules ();
  try handle_request ()
  with Failure msg -> Printexc.print_backtrace stderr; printf "<b>%s</b>" msg
