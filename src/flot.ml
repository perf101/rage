open Core.Std
open Buffer
open Printf
open Utils

type settings = {
  xaxis : axis_settings;
  yaxis : axis_settings;
}
and axis_settings = {
  min : float option;
  max : float option;
  tickFormatter : labels option;
  tickSize : float option;
  labelAngle : int option;
}
and labels = (float, string) Hashtbl.t

type point = float * float

type series = {
  label : string option;
  points : point list;
  color : int option;
  point_settings : point_settings;
  line_settings : line_settings;
}
and point_settings = {
  show_points : bool option;
}
and line_settings = {
  show_lines : bool option;
}

type plot = {
  dom_id : string;
  data : series list;
  settings : settings;
}

let rec settings_default = {
  xaxis = axis_default;
  yaxis = axis_default;
}
and axis_default = {
  min = None;
  max = None;
  tickFormatter = None;
  tickSize = None;
  labelAngle = None;
}

let rec series_default = {
  label = None;
  points = [];
  color = None;
  point_settings = point_settings_default;
  line_settings = line_settings_default;
}
and point_settings_default = {
  show_points = Some true;
}
and line_settings_default = {
  show_lines = Some false;
}

let rec tf_of_settings_to_buffer b {xaxis; yaxis} =
  tf_of_axis_to_buffer b "x" xaxis;
  tf_of_axis_to_buffer b "y" yaxis
and tf_of_axis_to_buffer b axis {tickFormatter} =
  match tickFormatter with None -> () | Some labels ->
  let axis_labels = axis ^ "_labels" in
  bprintf b "var %s = new Object();\n" axis_labels;
  let map_release ~key:i ~data:l =
    bprintf b "%s[%f] = '%s';\n" axis_labels i l in
  Float.Table.iter labels ~f:map_release;
  bprintf b "var %s_tf = function(val, axis) {" axis_labels;
  bprintf b "var r = %s[val]; " axis_labels;
  add_string b "return (typeof r === 'undefined') ? '' : r;}"

let sub_to_buffer b name ~f s =
  bprintf b "%s: {" name; f s; add_string b "},"

let opt_f b opt f =
  match opt with None -> () | Some v -> f v; add_string b ","

let rec settings_to_buffer b {xaxis; yaxis} =
  sub_to_buffer b "xaxis"  ~f:(axis_to_buffer b "x") xaxis;
  sub_to_buffer b "yaxis"  ~f:(axis_to_buffer b "y") yaxis
and axis_to_buffer b axis {min; max; tickFormatter; tickSize; labelAngle} =
  opt_f b min (fun m -> bprintf b "min: %f" m);
  opt_f b max (fun m -> bprintf b "max: %f" m);
  opt_f b tickFormatter
    (fun _ -> bprintf b "tickFormatter: %s_labels_tf" axis);
  opt_f b tickSize (fun ts -> bprintf b "tickSize: %f" ts);
  opt_f b labelAngle (fun la -> bprintf b "labelAngle: %d" la)

let settings_to_buffer b s =
  add_string b "{\n"; settings_to_buffer b s; add_string b "}"

let line_settings_to_buffer b {show_lines} =
  add_string b "lines: {";
  bprintf b "show:%b," (Option.value show_lines ~default:false);
  add_string b "},"

let rec series_to_buffer b series =
  add_string b "{";
  opt_f b series.label (fun l -> bprintf b "label:\"%s\"" l);
  add_string b "data:[";
  List.iter series.points ~f:(fun (x, y) -> bprintf b "[%f,%f]," x y);
  add_string b "],";
  opt_f b series.color (fun c -> bprintf b "color:%d" c);
  sub_to_buffer b "points" ~f:(point_settings_to_buffer b) series.point_settings;
  line_settings_to_buffer b series.line_settings;
  add_string b "},\n"
and point_settings_to_buffer b {show_points} =
  opt_f b show_points (fun s -> bprintf b "show: %b" s)

let string_of_plot (plot : plot) =
  let b = Buffer.create 2048 in
  let style = "width: 1000px; height: 600px" in
  bprintf b "<div id='%s' style='%s'></div>\n" plot.dom_id style;
  add_string b "<script type='text/javascript'>\n";
  add_string b "$(function () {\n";
  tf_of_settings_to_buffer b plot.settings;
  bprintf b "\n$.plot($('#%s'), [\n" plot.dom_id;
  List.iter plot.data ~f:(series_to_buffer b);
  add_string b "], ";
  settings_to_buffer b plot.settings;
  add_string b ");\n});\n</script>\n";
  contents b
