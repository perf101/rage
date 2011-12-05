open Core.Std
open Buffer
open Printf
open Utils

type setting =
  | Points of point_setting list
  | X_axis of axis_setting list
  | Y_axis of axis_setting list
and point_setting =
  | Show of bool
and axis_setting =
  | Min of float
  | Max of float
  | TickFormatter of string
  | TickSize of float

type settings = setting list

type labels = (float, string) Hashtbl.t

type datum = float * float

type data = datum list

let default_settings : settings = []

let default_labels : labels = Float.Table.create ()

let string_of_settings name ~f ss =
  let strings = List.map ~f ss in
  let body = String.concat ~sep:", " strings in
  sprintf "%s: {%s}" name body

let rec string_of_setting = function
  | Points ss -> string_of_settings "points" ~f:string_of_points ss
  | X_axis ss -> string_of_settings "xaxis"  ~f:string_of_axis   ss
  | Y_axis ss -> string_of_settings "yaxis"  ~f:string_of_axis   ss
and string_of_points = function
  | Show b -> sprintf "show: %b" b
and string_of_axis = function
  | Min n -> sprintf "min: %f" n
  | Max n -> sprintf "max: %f" n
  | TickFormatter fn -> sprintf "tickFormatter: %s" fn
  | TickSize f -> sprintf "tickSize: %f" f

let string_of_settings ss =
  let soss = List.map ~f:string_of_setting ss in
  let body = String.concat ~sep:",\n" soss in
  sprintf "var options = {\n%s}" (indent 2 body)

let insert_data b (series_n, data) =
  bprintf b "  var data_%d = [" series_n;
  List.iter data ~f:(fun (x, y) -> bprintf b "[%f,%f]," x y);
  add_string b "];\n"

let insert_instructions b settings labels series_nos =
  let append s = bprintf b "  %s\n" s in
  append "var releases = new Object();";
  let map_release ~key:i ~data:n = bprintf b "  releases[%f] = '%s';\n" i n in
  Float.Table.iter labels ~f:map_release;
  append "var tf = function(val, axis) {";
  append "  var r = releases[val];";
  append "  return (typeof r === 'undefined') ? '' : r;\n  }";
  add_string b (indent 2 (string_of_settings settings));
  let dss = List.map series_nos
    ~f:(fun i -> sprintf "{data: data_%d, label: \"config_id = %d\"}" i i) in
  let ds = String.concat ~sep:", " dss in
  bprintf b "\n$.plot($('#graph'), [%s], options);" ds

let plot ?(settings = default_settings) ?(labels = default_labels)
         (data_sets : (int * data) list) =
  let b = Buffer.create 1000 in
  add_string b "<div id='graph' style='width: 1000px; height: 600px'></div>\n";
  add_string b "<script type='text/javascript'>\n";
  add_string b " $(function () {\n";
  List.iter data_sets ~f:(insert_data b);
  let series_nos = fst (List.split data_sets) in
  insert_instructions b settings labels series_nos;
  add_string b " });\n";
  add_string b "</script>\n";
  indent 2 (contents b)
