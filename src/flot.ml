open Core.Std
open Buffer
open Printf
open Utils

type labels = (float, string) Hashtbl.t

type setting =
  | Points of point_setting list
  | X_axis of axis_setting list
  | Y_axis of axis_setting list
and point_setting =
  | Show of bool
and axis_setting =
  | Min of float
  | Max of float
  | TickFormatter of labels
  | TickSize of float

type settings = setting list

type point = float * float

type series = {
  label : string;
  points : point list;
}

type plot = {
  dom_id : string;
  data : series list;
  settings : settings;
}

let default_settings : settings = []

let default_labels : labels = Float.Table.create ()

let string_of_settings name ~f ss =
  let strings = List.map ~f ss in
  let body = String.concat ~sep:", " strings in
  sprintf "%s: {%s}" name body

let rec tick_formatters_of_setting b = function
  | X_axis ss -> List.iter ss ~f:(tick_formatters_of_axis b "x")
  | Y_axis ss -> List.iter ss ~f:(tick_formatters_of_axis b "y")
  | _ -> ()
and tick_formatters_of_axis b axis = function
  | TickFormatter labels ->
      let axis_labels = axis ^ "_labels" in
      bprintf b "var %s = new Object();\n" axis_labels;
      let map_release ~key:i ~data:l =
        bprintf b "  %s[%f] = '%s';\n" axis_labels i l in
      Float.Table.iter labels ~f:map_release;
      bprintf b "var %s_tf = function(val, axis) {" axis_labels;
      bprintf b "  var r = %s[val];" axis_labels;
      bprintf b "  return (typeof r === 'undefined') ? '' : r;\n  }"
  | _ -> ()

let rec string_of_setting = function
  | Points ss -> string_of_settings "points" ~f:string_of_points     ss
  | X_axis ss -> string_of_settings "xaxis"  ~f:(string_of_axis "x") ss
  | Y_axis ss -> string_of_settings "yaxis"  ~f:(string_of_axis "y") ss
and string_of_points = function
  | Show b -> sprintf "show: %b" b
and string_of_axis axis = function
  | Min n -> sprintf "min: %f" n
  | Max n -> sprintf "max: %f" n
  | TickFormatter _ -> sprintf "tickFormatter: %s_labels_tf" axis
  | TickSize f -> sprintf "tickSize: %f" f

let string_of_settings ss =
  let soss = List.map ~f:string_of_setting ss in
  let body = String.concat ~sep:",\n" soss in
  sprintf "{\n%s}" (indent 2 body)

let string_of_series {label; points} =
  let pss = List.map points ~f:(fun (x, y) -> sprintf "[%f,%f]" x y) in
  let ps = String.concat ~sep:"," pss in
  "{label:\"" ^ label ^ "\",data:[" ^ ps ^ "]}"

let string_of_plot (plot : plot) =
  let b = Buffer.create 1000 in
  let style = "width: 1000px; height: 600px" in
  bprintf b "<div id='%s' style='%s'></div>\n" plot.dom_id style;
  add_string b "<script type='text/javascript'>\n";
  add_string b " $(function () {\n";
  List.iter plot.settings ~f:(tick_formatters_of_setting b);
  let dss = List.map plot.data ~f:string_of_series in
  let ds = String.concat ~sep:",\n" dss in
  let ss = string_of_settings plot.settings in
  bprintf b "\n$.plot($('#%s'), [\n%s\n], %s);" plot.dom_id ds ss;
  add_string b " });\n";
  add_string b "</script>\n";
  indent 2 (contents b)
