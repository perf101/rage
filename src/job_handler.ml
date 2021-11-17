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
        let%bind tc_fqn, tc_config_tbl = get_tc_config_tbl_name conn som_id
        and som_config_tbl, som_tbl_exists = som_config_tbl_exists ~conn som_id in
        let columns = Printf.sprintf "tc_config.*, machines.*, %s.*" tc_config_tbl ^ (if som_tbl_exists then "," ^ som_config_tbl ^ ".*" else "") in
        let joins = [ "tc_config"
          ; "machines ON tc_config.machine_id=machines.machine_id"
          ; Printf.sprintf "%s ON %s.tc_config_id=tc_config.tc_config_id" tc_config_tbl tc_config_tbl
        ] @ (if som_tbl_exists then [
          Printf.sprintf "soms_jobs ON (soms_jobs.som_id=%d AND soms_jobs.job_id=%d)" som_id job
          ; Printf.sprintf "measurements_2 ON (measurements_2.som_job_id=soms_jobs.id AND measurements_2.tc_config_id=tc_config.tc_config_id AND measurements_2.tc_config_id=%s.tc_config_id)" tc_config_tbl
          ; Printf.sprintf "%s ON measurements_2.som_config_id=%s.som_config_id" som_config_tbl som_config_tbl
        ] else []) in
        let query = Printf.sprintf "SELECT DISTINCT %s FROM %s WHERE tc_config.job_id=%d AND tc_config.tc_fqn='%s'" columns (String.concat ~sep:" INNER JOIN " joins) job tc_fqn
        in
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
