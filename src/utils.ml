open Printf

let indent n text =
   let prefix = String.make n ' ' in
   let lines = Str.split (Str.regexp "\n") text in
   let indented_lines = List.map (fun line -> prefix ^ line) lines in
   (String.concat "\n" indented_lines) ^ "\n"

let cat filename =
  let chan = open_in filename in
  try
    while true; do
      print_endline (input_line chan)
    done
  with End_of_file ->
    close_in chan

let print_row (tag : string) (row : string list) : unit =
  print_string "   <tr>";
  List.iter (fun elem -> printf "<%s>%s</%s>" tag elem tag) row;
  print_endline "</tr>"

let print_table result =
  print_endline "  <table border='1'>";
  print_row "th" result#get_fnames_lst;
  List.iter (fun row -> print_row "td" row) result#get_all_lst;
  print_endline "  </table>"

let natural_map_from_list l =
  let map = Hashtbl.create (List.length l) in
  let rec add n xs =
    match xs with
      | [] -> ()
      | x::xs -> Hashtbl.add map n x; add (n+1) xs
  in add 1 l;
  map

let reverse_map m =
  let m' = Hashtbl.create (Hashtbl.length m) in
  Hashtbl.iter (fun k v -> Hashtbl.add m' v k) m;
  m'
