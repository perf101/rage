open Core.Std
open Utils

let t ~args = object (self)
  inherit Html_handler.t ~args

  method private write_body =
    let query = "SELECT report_id, report_desc FROM reports" in
    let result = Sql.exec_exn ~conn ~query in
    let print_report report =
      let id = report.(0) in
      let desc = report.(1) in
      printf "<tr>";
      printf "<td>%s</td>" id;
      printf "<td class='encoded'>%s</td>" desc;
      printf "<td><a href='/?p=report_page&id=%s'>View</a></td>" id;
      printf "<td><a href='/?p=report_generator_page&id=%s'>Edit</a></td>" id;
      printf "<td><a href='/?p=report_clone&id=%s'>Clone</a></td>" id;
      printf "<td><a href='/?p=report_delete&id=%s'>Delete</a></td>" id;
      printf "</tr>";
    in
    printf "<table border='1'>\n";
    printf "<tr><th>ID</th><th>Description</th><th colspan='4'>Actions</th></tr>";
    Array.iter ~f:print_report result#get_all;
    printf "</table>\n";
    printf "<a href='/?p=report_generator_page'>New</a>\n";
    self#include_javascript
end
