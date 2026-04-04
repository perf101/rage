open Core
open Async

let t ~args = object (self)
  inherit Json_handler.t ~args

  method private write_body =
    let url = self#get_param_exn "url" in
    let tuples = [("url", url)] in
    let%map id = Postgresql_async.wrap_sql ~conn (Sql.ensure_inserted_get_id ~tbl:"tiny_urls" ~tuples) in
    printf "{\"id\":%d}" id
end
