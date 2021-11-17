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
    let json_of_row row =
      let cols = row#get_fnames_lst in
      row#get_tuple_lst 0 |> List.map ~f:json_of_field
      |> List.zip_exn cols |> Ezjsonm.dict
    in
    let%bind build = Postgresql_async.exec_exn ~conn ~query
    and som_ids =
      let query = Printf.sprintf "SELECT som_id FROM som_jobs WHERE job_id=%d" job in
      let%map result = Postgresql_async.exec_exn ~conn ~query in
      result#get_all_lst |> List.concat |> List.map ~f:int_of_string
    in
    let%bind tc_configs =
      som_ids |> Deferred.List.map ~how:`Parallel ~f:(fun som_id ->
        let%bind tc_fqn, tc_config = get_tc_config_tbl_name conn som_id in
        let query = Printf.sprintf "SELECT * FROM tc_config INNER JOIN %s ON
        tc_config.tc_config_id=%s.tc_config_id WHERE job_id=%d AND tc_fqn=%s"
          tc_config tc_config job tc_fqn in
        Postgresql_async.exec_exn ~conn ~query
      )
    in
    Ezjsonm.dict [
      "build", json_of_row build;
      "soms", Ezjsonm.list Ezjsonm.int som_ids;
      "tc_config", Ezjsonm.list json_of_row tc_configs
    ] |> Ezjsonm.value_to_channel ~minify:false stdout;
    return ()
end
