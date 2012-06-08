open Core.Std
open Postgresql

let debug msg = output_string stderr (msg ^ "\n")

module ListKey = struct
  module T = struct
    type t = string list with sexp
    type sexpable = t
    let compare = compare
    let equal = (=)
    let hash = Hashtbl.hash
  end
  include T
  include Hashable.Make (T)
end

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

let merge_table_into src dst =
  String.Table.merge_into ~src ~dst
    ~f:(fun ~key src_v dst_v_opt ->
      match dst_v_opt with None -> Some src_v | vo -> vo)

let cat filename =
  print_string (In_channel.with_file ~f:In_channel.input_all filename)

let exec_query (conn : connection) (query : string) : result option =
  let result = conn#exec query in
  match result#status with
    | Command_ok | Tuples_ok -> Some result
    | _ -> debug conn#error_message; None

let exec_query_exn (conn : connection) (query : string) : result =
  match exec_query conn query with
  | None -> failwith ("Failed to execute query: " ^ query)
  | Some result -> result

let get_first_entry r =
  if r#nfields > 0 && r#ntuples > 0
  then Some (String.strip (r#getvalue 0 0))
  else None

let get_first_entry_exn r =
  match get_first_entry r with
  | None -> failwith "get_first_entry_exn"
  | Some v -> v

let get_col result col =
  Array.to_list (Array.map ~f:(fun row -> row.(col)) result#get_all)

let get_first_col result = get_col result 0

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
  let print_option (l, v) =
    printf "<option value='%s'" v;
    if List.mem ~set:selected l then printf " selected='selected'";
    printf ">%s</option>\n" l
  in List.iter options ~f:print_option;
  printf "</select>\n";
  if td then printf "</td>\n"

let print_select_list ?(td=false) ?(label="") ?(selected=[]) ?(attrs=[]) l =
  print_select ~td ~label ~selected ~attrs (List.map l ~f:(fun x -> (x, x)))

let get_options_for_field db_result col =
  let nRows = db_result#ntuples - 1 in
  let ftype = db_result#ftype col in
  let data = db_result#get_all in
  let rec aux acc = function
    | -1 -> acc
    | i ->
      let elem =
        if db_result#getisnull i col then "(NULL)" else data.(i).(col)
      in aux (elem::acc) (i-1)
  in
  let cmp x y =
    try
      if ftype = Postgresql.INT4
      then compare (int_of_string x) (int_of_string y)
      else compare x y
    with _ -> output_string stderr (x ^ ", " ^ y ^ "\n"); 0
  in
  List.sort ~cmp (List.dedup (aux [] nRows))

let print_options_for_field namespace db_result col =
  let fname = db_result#fname col in
  let opts = get_options_for_field db_result col in
  let form_name = sprintf "%s_%s" namespace fname in
  printf "<table border='1' class='filter_table'>";
  printf "<tr><th>%s</th></tr><tr>" fname;
  print_select_list ~td:true ~selected:["ALL"]
    ~attrs:[("name", form_name); ("multiple", "multiple"); ("size", "3");
            ("class", "multiselect")]
    ("ALL"::opts);
  printf "</tr></table>"

let print_options_for_fields conn tbl namespace =
  let query = "SELECT * FROM " ^ tbl in
  let result = exec_query_exn conn query in
  List.iter ~f:(print_options_for_field namespace result)
    (List.range 1 result#nfields);
  printf "<br style='clear: both' />\n"
