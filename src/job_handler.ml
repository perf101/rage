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
    let of_row ~cols row =
        let cmp (k1, _) (k2, _) = String.compare k1 k2 in
        row |> List.map ~f:json_of_field |> List.zip_exn cols
        |> List.sort ~compare:cmp |> Ezjsonm.dict
    in
    let json_of_row row =
      let cols = row#get_fnames_lst in
      row#get_tuple_lst 0 |> of_row ~cols
    in
    let json_of_rows rows =
      let cols = rows#get_fnames_lst in
      Ezjsonm.list (of_row ~cols) rows#get_all_lst
    in
    let%bind build = Postgresql_async.exec_exn ~conn ~query
    and som_ids =
      let query = Printf.sprintf "SELECT som_id FROM soms_jobs WHERE job_id=%d" job in
      let%map result = Postgresql_async.exec_exn ~conn ~query in
      result#get_all_lst |> List.concat |> List.map ~f:int_of_string
    in
    let%bind tc_configs =
      som_ids |> Deferred.List.map ~how:`Parallel ~f:(fun som_id ->
        let%bind tc_fqn, tc_config = get_tc_config_tbl_name conn som_id in
        let query = Printf.sprintf "SELECT * FROM tc_config INNER JOIN machines ON machines.machine_id=tc_config.machine_id LEFT JOIN %s ON tc_config.tc_config_id=%s.tc_config_id WHERE job_id=%d AND tc_fqn='%s'"
          tc_config tc_config job tc_fqn in
        let %map result = Postgresql_async.exec_exn ~conn ~query in
        som_id, result
      )
    in
    Ezjsonm.dict [
      "build", json_of_row build;
      "tc_config", tc_configs |> List.map ~f:(fun (k, r) -> string_of_int k, json_of_rows r) |> Ezjsonm.dict
    ] |> Ezjsonm.value_to_channel ~minify:false stdout;
    return ()
end
