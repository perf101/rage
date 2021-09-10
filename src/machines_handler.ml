open Core
open Async
open Utils

let t ~args = object (self)
  inherit Json_handler.t ~args

  method private write_body =
		let fields = [
     "machine_type";
     "cpu_model";
     "number_of_cpus"; "cpu_vendor"; "cpu_family"; "cpu_model_int"; "cpu_stepping";
     "cpu_speed"
    ] in
    let query = "SELECT machine_name, " ^ (String.concat ~sep:"," fields) ^ " FROM machines ORDER BY machine_name" in
    let json_of_field = function
      | "" -> Ezjsonm.unit ()
      | s -> Ezjsonm.string s in
    let json_of_machine r =
      r.(0), List.zip_exn fields (r |> Array.to_list |> List.tl_exn |> List.map ~f:json_of_field)
      |> Ezjsonm.dict |> Ezjsonm.value
    in
    let%bind machines = Postgresql_async.exec_exn ~conn ~query in
    Array.map ~f:json_of_machine machines#get_all
    |> Array.to_list |> Ezjsonm.dict |> Ezjsonm.value
    |> Ezjsonm.value_to_channel ~minify:false stdout;
    return ()
end
