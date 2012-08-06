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
    ~f:(fun ~key:_ src_v dst_v_opt ->
      match dst_v_opt with None -> Some src_v | vo -> vo)

let cat filename =
  print_string (In_channel.with_file ~f:In_channel.input_all filename)

(* DATABASE INTERACTION *)

let get_value r row col null_val =
  if r#getisnull row col then null_val else r#getvalue row col

let combine_maps conn tbls f =
  let m = String.Table.create () in
  List.iter tbls ~f:(fun t -> merge_table_into (f conn t) m);
  m

let get_column_types conn tbl =
  String.Table.of_alist_exn (Sql.get_col_types_lst ~conn ~tbl)

let get_column_types_many conn tbls = combine_maps conn tbls get_column_types

let get_column_fqns conn tbl =
  let col_names = Sql.get_col_names ~conn ~tbl in
  let nameToFqn = String.Table.create () in
  let process_column name =
    let fqn = tbl ^ "." ^ name in
    String.Table.replace nameToFqn ~key:name ~data:fqn
  in List.iter col_names ~f:process_column;
  nameToFqn

let get_column_fqns_many conn tbls = combine_maps conn tbls get_column_fqns

let html_to_text html =
  let rules =
    let pairs = [("%20", " "); ("%2C", ","); ("+", " ")] in
    List.map pairs ~f:(fun (r, t) -> (Str.regexp r, t))
  in
  List.fold_left rules ~init:html
    ~f:(fun h (r, t) -> Str.global_replace r t h)

let extract_filter col_fqns col_types params key_prefix =
  let m = String.Table.create () in
  let update_m v vs_opt =
    let vs = Option.value vs_opt ~default:[] in Some (v::vs) in
  let filter_insert (k, v) =
    if v = "ALL" then () else
    if String.is_prefix k ~prefix:key_prefix then begin
      let k2 = String.chop_prefix_exn k ~prefix:key_prefix in
      String.Table.change m k2 (update_m v)
    end in
  List.iter params ~f:filter_insert;
  let l = String.Table.to_alist m in
  let conds = List.map l
    ~f:(fun (k, vs) ->
      let vs = List.map vs ~f:html_to_text in
      let has_null = List.mem ~set:vs "(NULL)" in
      let vs = if has_null then List.filter vs ~f:((<>) "(NULL)") else vs in
      let ty = String.Table.find_exn col_types k in
      let quote = Sql.Type.is_quoted ty in
      let vs_oq =
        if quote then List.map vs ~f:(fun v -> "'" ^ v ^ "'") else vs in
      let fqn = String.Table.find_exn col_fqns k in
      let val_list = concat vs_oq in
      let in_cond = sprintf "%s IN (%s)" fqn val_list in
      let null_cond = if has_null then sprintf "%s IS NULL" fqn else "" in
      if List.is_empty vs then null_cond else
      if has_null then sprintf "(%s OR %s)" in_cond null_cond else
      in_cond
    ) in
  concat ~sep:" AND " conds

(* PRINTING HTML *)

let print_col_default _ _ tag data =
  printf "<%s>%s</%s>" tag data tag

let print_row_custom ?(print_col = print_col_default) row_i tag row =
  print_string "   <tr>";
  List.iteri row ~f:(fun col_i data -> print_col row_i col_i tag data);
  print_endline "</tr>"

let print_row_default row_i tag row =
  print_row_custom ~print_col:print_col_default row_i tag row

let print_row_header row =
  print_row_default 0 "th" row

let print_table_custom_row print_row result =
  print_endline "  <table border='1'>";
  print_row (-1) "th" result#get_fnames_lst;
  List.iteri result#get_all_lst
    ~f:(fun row_i row -> print_row row_i "td" row);
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
    with _ -> 0
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
  printf "</tr><tr>";
  print_select ~td:true ~selected:["SPLIT_BY_GRAPH"]
    ~attrs:[("name", form_name ^ "_split")]
    [("SPLIT BY GRAPH", "split_by_graph"); ("SPLIT BY LINE", "split_by_line");
     ("DON'T SPLIT", "dont_split")];
  printf "</tr></table>"

let print_options_for_fields conn tbl namespace =
  let query = "SELECT * FROM " ^ tbl in
  let result = Sql.exec_exn ~conn ~query in
  List.iter ~f:(print_options_for_field namespace result)
    (List.range 1 result#nfields);
  printf "<br style='clear: both' />\n"
