open Core.Std

let indent n text =
   let prefix = String.make n ' ' in
   let lines = String.split ~on:'\n' text in
   let indented_lines = List.map lines (fun line -> prefix ^ line) in
   String.concat ~sep:"\n" indented_lines

let cat filename =
  print_string (In_channel.with_file ~f:In_channel.input_all filename)

let print_row (tag : string) (row : string list) : unit =
  print_string "   <tr>";
  List.iter row (fun elem -> printf "<%s>%s</%s>" tag elem tag);
  print_endline "</tr>"

let print_table result =
  print_endline "  <table border='1'>";
  print_row "th" result#get_fnames_lst;
  List.iter result#get_all_lst (fun row -> print_row "td" row);
  print_endline "  </table>"

let natural_map_from_list l =
  let ints = List.range 1 ((List.length l) + 1) in
  Int.Table.of_alist_exn (List.combine_exn ints l)

let reverse_natural_map m =
  let m' = String.Table.create ~size:(Int.Table.length m) () in
  Int.Table.iter m ~f:(fun ~key:k ~data:v -> String.Table.replace m' ~key:v ~data:k);
  m'
