open Core
open Async
open Utils

let t ~args = object (self)
  inherit Json_handler.t ~args

  method private write_body =
    let job = int_of_string (self#get_param_exn "job_id") in
    let query = Printf.sprintf "SELECT builds.* FROM builds INNER JOIN jobs ON builds.build_id=jobs.build_id WHERE job_id=%d" job in
    let json_of_field = function
      | "" -> Ezjsonm.unit ()
      | s -> Ezjsonm.string s in
    let json_of_build build =
      let values = build#get_tuple_lst 0 |> List.map ~f:json_of_field in
      List.zip_exn build#get_fnames_lst values |> Ezjsonm.dict |> Ezjsonm.value
    in
    let%bind build = Postgresql_async.exec_exn ~conn ~query in
    json_of_build build
    |> Ezjsonm.value_to_channel ~minify:false stdout;
    return ()
end
