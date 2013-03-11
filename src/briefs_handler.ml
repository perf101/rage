open Core.Std
open Utils

let t ~args = object (self)
  inherit Html_handler.t ~args

  method private write_body =
    let query = "SELECT brief_id, brief_desc, brief_params FROM briefs" in
    let result = Sql.exec_exn ~conn ~query in
    let print_brief brief =
      let id = brief.(0) in
      let desc = brief.(1) in
      let params = brief.(2) in
      printf "<tr>";
      printf "<td>%s</td>" id;
      printf "<td class='encoded'>%s</td>" desc;
      printf "<td>%s</td>" params;
      printf "<td><a href='/marcusg/?p=brief&id=%s'>View</a></td>" id;
      printf "<td><a href='/?p=brief_generator_page&id=%s'>Edit</a></td>" id;
      printf "<td><a href='/?p=brief_clone&id=%s'>Clone</a></td>" id;
      printf "<td><a href='/?p=brief_delete&id=%s'>Delete</a></td>" id;
      printf "</tr>";
    in
    printf "<table border='1'>\n";
    printf "<tr><th>ID</th><th>Description</th><th colspan='4'>Parameters</th></tr>";
    Array.iter ~f:print_brief result#get_all;
    printf "</table>\n";
    printf "<a href='/?p=report_generator_page'>New</a>\n";
    self#include_javascript
end
