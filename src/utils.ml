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

(* DATABASE INTERACTION *)

let exec_query (conn : connection) (query : string) : result option =
  let result = conn#exec query in
  match result#status with
    | Command_ok | Tuples_ok -> Some result
    | _ -> debug conn#error_message; None

let exec_query_exn (conn : connection) (query : string) : result =
  match exec_query conn query with
  | None -> failwith ("Failed to execute query: " ^ query)
  | Some result -> result

let exec_sql_exn (conn : connection) (query : string) : unit =
  ignore (exec_query_exn conn query)

let get_first_entry r =
  if r#nfields > 0 && r#ntuples > 0
  then Some (String.strip (r#getvalue 0 0))
  else None

let get_first_entry_exn r =
  match get_first_entry r with
  | None -> failwith "get_first_entry_exn"
  | Some v -> v

let extract_column rows col =
  Array.to_list (Array.map ~f:(fun row -> row.(col)) rows)

let get_col result col = extract_column result#get_all col

let get_first_col result = get_col result 0

let get_value r row col null_val =
  if r#getisnull row col then null_val else r#getvalue row col

let combine_maps conn tbls f =
  let m = String.Table.create () in
  List.iter tbls ~f:(fun t -> merge_table_into (f conn t) m);
  m

let get_column_types conn tbl =
  let query =
    ("SELECT column_name, data_type FROM information_schema.columns ") ^
    (sprintf "WHERE table_name='%s'" tbl) in
  let col_types_data = exec_query_exn conn query in
  let nameToType = String.Table.create () in
  let process_column nameTypeList =
    let name = List.nth_exn nameTypeList 0 in
    let ty = List.nth_exn nameTypeList 1 in
    String.Table.replace nameToType ~key:name ~data:ty
  in List.iter col_types_data#get_all_lst ~f:process_column;
  nameToType

let get_column_types_many conn tbls = combine_maps conn tbls get_column_types

let is_db_type_quotable ty =
  List.mem ~set:["character varying"; "boolean"; "text"] ty

let is_db_field_quotable col_types name =
  let ty_opt = String.Table.find col_types name in
  Option.value_map ty_opt ~default:false ~f:is_db_type_quotable

let get_column_fqns conn tbl =
  let query =
    ("SELECT column_name FROM information_schema.columns ") ^
    (sprintf "WHERE table_name='%s'" tbl) in
  let col_names = get_first_col (exec_query_exn conn query) in
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
      let quote = is_db_field_quotable col_types k in
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

type sql_meta_type = Numeric (* not quoted *) | Non_numeric (* quoted *)

let opt_quote s quote_rule =
  match quote_rule with Numeric -> s | Non_numeric -> "'" ^ s ^ "'"

let sql_equals_of_tuples tuples =
  let pair (name, value, quote) =
    sprintf "%s=%s" name (opt_quote value quote)
  in List.map ~f:pair tuples

let sql_cond_of_tuples tuples =
  String.concat ~sep:" AND " (sql_equals_of_tuples tuples)

let sql_names_values_of_tuples tuples =
  let fst (name, _, _) = name in
  let qt (_, value, quote) = opt_quote value quote in
  let names = String.concat ~sep:"," (List.map ~f:fst tuples) in
  let values = String.concat ~sep:"," (List.map ~f:qt tuples) in
  sprintf "(%s) VALUES (%s)" names values

let insert_and_get_first_col conn tbl tuples =
  (* let d = sprintf "%-25s: %s" tbl (string_of_sql_tuples tuples) in *)
  let insert = match tuples with
    | [] ->
        (* There's only one column in the table -- the (serial) ID column *)
        sprintf "INSERT INTO %s VALUES (DEFAULT)" tbl
    | _ ->
        let values = sql_names_values_of_tuples tuples in
        sprintf "INSERT INTO %s %s" tbl values
  in
  let select = match tuples with
    | [] -> sprintf "SELECT * FROM %s" tbl
    | _ ->
        let cond = sql_cond_of_tuples tuples in
        sprintf "SELECT * FROM %s WHERE %s" tbl cond
  in
  exec_sql_exn conn insert;
  (*debug ("INSERTED: " ^ d);*)
  get_first_entry_exn (exec_query_exn conn select)

(* let ensure_inserted conn tbl tuples = *)
(*   ignore (ensure_inserted_get_first_col conn tbl tuples) *)

(* PRINTING HTML *)

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
