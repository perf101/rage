open Core
open Async

let t ~args = object (self)
  inherit Html_handler.t ~args

  method handle =
    let id = int_of_string (self#get_param_exn "t") in
    let query = sprintf "SELECT url FROM tiny_urls WHERE key=%d" id in
    let%map result = Postgresql_async.exec_exn ~conn ~query in
    match result#ntuples with
    | 1 -> self#javascript_redirect (Sql.get_first_entry_exn ~result)
    | _ -> self#write_404
end
