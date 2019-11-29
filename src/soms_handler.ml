open Core
open Utils

let t ~args = object (self)
  inherit Json_handler.t ~args

  method private write_body =
    let query = "SELECT tc_fqn,description FROM test_cases ORDER BY tc_fqn" in
    let tcs = Sql.exec_exn ~conn ~query in
    let json_of_tc tc =
      sprintf "\"%s\":{\"desc\":\"%s\"}" tc.(0) tc.(1) in
    let tcs_json = concat_array (Array.map ~f:json_of_tc tcs#get_all) in
    let query = "SELECT som_id,som_name,tc_fqn FROM soms ORDER BY som_id" in
    let soms = Sql.exec_exn ~conn ~query in
    let json_of_som som =
      sprintf "\"%s\":{\"name\":\"%s\",\"tc\":\"%s\"}" som.(0) som.(1) som.(2) in
    let soms_json = concat_array (Array.map ~f:json_of_som soms#get_all) in
    printf "{\"tcs\":{%s},\"soms\":{%s}}" tcs_json soms_json
end
