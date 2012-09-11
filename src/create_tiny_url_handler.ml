open Core.Std
open Utils

let t ~args = object (self)
  inherit Json_handler.t ~args

  method private write_body =
    let url = self#get_param_exn "url" in
    let tuples = [("url", url)] in
    let id = Sql.ensure_inserted_get_id ~conn ~tbl:"tiny_urls" ~tuples in
    printf "{\"id\":%d}" id
end
