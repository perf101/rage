open Core.Std

let indent n text =
   let prefix = String.make n ' ' in
   let lines = String.split ~on:'\n' text in
   let indented_lines = List.map lines (fun line -> prefix ^ line) in
   String.concat ~sep:"\n" indented_lines

let cat filename =
  print_string (In_channel.with_file ~f:In_channel.input_all filename)

let print_col_default row_i col_i tag data =
  printf "<%s>%s</%s>" tag data tag

let print_row_custom ?(print_col = print_col_default) row_i tag row =
  print_string "   <tr>";
  List.iteri row (fun col_i data -> print_col row_i col_i tag data);
  print_endline "</tr>"

let print_row_default row_i tag row =
  print_row_custom ~print_col:print_col_default row_i tag row

let print_row_header row =
  print_row_default 0 "th" row

let print_table_custom_row print_row result =
  print_endline "  <table border='1'>";
  print_row (-1) "th" result#get_fnames_lst;
  List.iteri result#get_all_lst
    (fun row_i row -> print_row row_i "td" row);
  print_endline "  </table>"

let print_table_custom_col print_col result =
  print_endline "  <table border='1'>";
  print_row_custom ~print_col (-1) "th" result#get_fnames_lst;
  List.iteri result#get_all_lst
    (fun row_i row -> print_row_custom ~print_col row_i "td" row);
  print_endline "  </table>"

let print_table result =
  print_table_custom_row print_row_default result

let natural_map_from_list l =
  let len = float_of_int (List.length l) in
  let ints = List.frange 1. (len +. 1.) in
  Float.Table.of_alist_exn (List.combine_exn ints l)

let reverse_natural_map m =
  let m' = String.Table.create ~size:(Float.Table.length m) () in
  Float.Table.iter m ~f:(fun ~key:k ~data:v -> String.Table.replace m' ~key:v ~data:k);
  m'
