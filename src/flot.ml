open Core.Std
open Buffer
open Printf
open Utils

type setting =
  | X_axis of axis_setting list
  | Y_axis of axis_setting list
and axis_setting =
  | Min of int
  | Max of int
  | TickFormatter of string

type settings = setting list

type labels = (int, string) Hashtbl.t

type datum = int * int

type data = datum list

let default_settings : settings = []

let default_labels : labels = Int.Table.create ()

let rec string_of_setting s =
  match s with
    | X_axis ss -> print_axis "xaxis" ss
    | Y_axis ss -> print_axis "yaxis" ss
and print_axis name ss =
    let axis_settings = List.map ~f:string_of_axis_setting ss in
    let body = String.concat ~sep:", " axis_settings in
    sprintf "%s: {%s}" name body
and string_of_axis_setting a =
  match a with
    | Min n -> sprintf "min: %d" n
    | Max n -> sprintf "max: %d" n
    | TickFormatter f -> sprintf "tickFormatter: %s" f

let string_of_settings ss =
  let soss = List.map ~f:string_of_setting ss in
  let body = String.concat ~sep:",\n" soss in
  sprintf "var options = {\n%s}" (indent 2 body)

let insert_data b (data : (int * int) list) =
  add_string b "  var data = [";
  List.iter data ~f:(fun (x, y) -> bprintf b "[%d,%d]," x y);
  add_string b "];\n"

let insert_instructions b settings labels =
  let append s = bprintf b "  %s\n" s in
  append "var releases = new Object();";
  let map_release ~key:i ~data:n = bprintf b "  releases[%d] = '%s';\n" i n in
  Int.Table.iter labels ~f:map_release;
  append "var tf = function(val, axis) {";
  append "  var r = releases[val];";
  append "  return (typeof r === 'undefined') ? '' : r;\n  }";
  add_string b (indent 2 (string_of_settings settings));
  append "$.plot($('#graph'), [{data: data, points: {show: true}}], options);"

let plot ?(settings = default_settings) ?(labels = default_labels)
         (data : data) =
  let b = Buffer.create 1000 in
  add_string b "<div id='graph' style='width: 1000px; height: 600px'></div>\n";
  add_string b "<script type='text/javascript'>\n";
  add_string b " $(function () {\n";
  insert_data b data;
  insert_instructions b settings labels;
  add_string b " });\n";
  add_string b "</script>\n";
  indent 2 (contents b)
