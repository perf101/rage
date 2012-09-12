open Core.Std
open Utils

let t ~args = object (self)
  inherit Html_handler.t ~args

  method private write_body =
    let query = "SELECT som_id, som_name FROM soms ORDER BY som_id" in
    let result = Sql.exec_exn ~conn ~query in
    let print_row row_i tag row =
      match row_i with -1 -> print_row_header row | _ ->
      print_string "   <tr>";
      let som_id = List.nth_exn row 0 in
      let name = List.nth_exn row 1 in
      printf "<%s>%s</%s>" tag som_id tag;
      printf "<%s><a href='?som=%s'>%s</a></%s>" tag som_id name tag;
      print_string "   </tr>" in
    print_table_custom_row print_row result;
end
