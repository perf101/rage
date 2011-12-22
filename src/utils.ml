open Core.Std

let index l x =
  let rec aux i = function
    | [] -> failwith "index []"
    | x'::xs -> if x = x' then i else aux (i+1) xs
  in aux 0 l

let rec concat ?(sep = ",") l =
  String.concat ~sep
    (List.filter l ~f:(fun s -> not (String.is_empty s)))

let rec concat_array ?(sep = ",") a =
  String.concat_array ~sep
    (Array.filter a ~f:(fun s -> not (String.is_empty s)))

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

let print_table result =
  print_table_custom_row print_row_default result

let print_select ?(td=false) ?(label="") ?(selected=[]) ?(attrs=[]) options =
  if td then printf "<td>\n";
  if label <> "" then printf "<b>%s</b>:\n" label;
  printf "<select";
  List.iter attrs ~f:(fun (k, v) -> printf " %s='%s'" k v);
  printf ">\n";
  let print_option o =
    printf "<option";
    if List.mem ~set:selected o then printf " selected='selected'";
    printf ">%s</option>\n" o
  in List.iter options ~f:print_option;
  printf "</select>\n";
  if td then printf "</td>\n"

let get_options_for_field data nRows col ftype =
  let rec aux acc = function
    | -1 -> acc
    | i -> aux (data.(i).(col)::acc) (i-1)
  in
  let cmp x y =
    if ftype = Postgresql.INT4
    then compare (int_of_string x) (int_of_string y)
    else compare x y
  in
  List.sort ~cmp (List.dedup (aux [] nRows))
