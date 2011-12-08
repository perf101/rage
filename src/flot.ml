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
  | TickFormatter of labels
  | TickSize of float
and labels = (float, string) Hashtbl.t

type point = float * float

type series = {
  label : string;
  points : point list;
}

type plot = {
  dom_id : string;
  data : series list;
  settings : setting list;
}

let rec tf_of_setting_to_buffer b = function
  | X_axis ss -> List.iter ss ~f:(tf_of_axis_to_buffer b "x")
  | Y_axis ss -> List.iter ss ~f:(tf_of_axis_to_buffer b "y")
  | _ -> ()
and tf_of_axis_to_buffer b axis = function
  | TickFormatter labels ->
      let axis_labels = axis ^ "_labels" in
      bprintf b "var %s = new Object();\n" axis_labels;
      let map_release ~key:i ~data:l =
        bprintf b "%s[%f] = '%s';\n" axis_labels i l in
      Float.Table.iter labels ~f:map_release;
      bprintf b "var %s_tf = function(val, axis) {" axis_labels;
      bprintf b "var r = %s[val]; " axis_labels;
      add_string b "return (typeof r === 'undefined') ? '' : r;}"
  | _ -> ()

let sub_settings_to_buffer b name ~f ss =
  bprintf b "%s: {" name;
  List.iter ~f:(fun s -> f s; add_string b ",") ss;
  add_string b "},"

let rec setting_to_buffer b = function
  | Points ss -> sub_settings_to_buffer b "points" ~f:(points_to_buffer b) ss
  | X_axis ss -> sub_settings_to_buffer b "xaxis"  ~f:(axis_to_buffer b "x") ss
  | Y_axis ss -> sub_settings_to_buffer b "yaxis"  ~f:(axis_to_buffer b "y") ss
and points_to_buffer b = function
  | Show s -> bprintf b "show: %b" s
and axis_to_buffer b axis = function
  | Min n -> bprintf b "min: %f" n
  | Max n -> bprintf b "max: %f" n
  | TickFormatter _ -> bprintf b "tickFormatter: %s_labels_tf" axis
  | TickSize f -> bprintf b "tickSize: %f" f

let settings_to_buffer b ss =
  add_string b "{\n";
  List.iter ~f:(setting_to_buffer b) ss;
  add_string b "}"

let series_to_buffer b {label; points} =
  bprintf b "{label:\"%s\",data:[" label;
  List.iter points ~f:(fun (x, y) -> bprintf b "[%f,%f]," x y);
  add_string b "]},\n"

let string_of_plot (plot : plot) =
  let b = Buffer.create 2048 in
  let style = "width: 1000px; height: 600px" in
  bprintf b "<div id='%s' style='%s'></div>\n" plot.dom_id style;
  add_string b "<script type='text/javascript'>\n";
  add_string b "$(function () {\n";
  List.iter plot.settings ~f:(tf_of_setting_to_buffer b);
  bprintf b "\n$.plot($('#%s'), [\n" plot.dom_id;
  List.iter plot.data ~f:(series_to_buffer b);
  add_string b "], ";
  settings_to_buffer b plot.settings;
  add_string b ");\n});\n</script>\n";
  contents b
