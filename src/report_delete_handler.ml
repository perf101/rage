open Core.Std
open Utils

let t ~args = object (self)
  inherit Html_handler.t ~args

  method handle =
    let id = int_of_string (self#get_param_exn "id") in
    let query = sprintf "DELETE FROM reports WHERE report_id = %d" id in
    Sql.exec_ign_exn ~conn ~query;
    self#javascript_redirect "/?p=reports"
end
