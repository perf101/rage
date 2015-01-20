open Core.Std
open Utils
open Unix

let importer = "/usr/groups/perfeng/bin/importer-xenrt"

(*
 * We accept either:
 *  - jobid from import_page form
 *  - JOBID from XenRT CALLBACK_URL
 *)

let import_job job_id =
  let cmd = Printf.sprintf "%s -job %d -ignoreseenjobs" importer job_id in
  let ic = Unix.open_process_in cmd in
  try
    while true do
      Printf.printf "%s\n" (input_line ic)
    done
  with End_of_file ->
    ignore (Unix.close_process_in ic)

let t ~args = object (self)
  inherit Html_handler.t ~args

  method private write_body =
    let job_id = try
        self#get_param_exn "jobid"
      with Not_found ->
        self#get_param_exn "JOBID"
    in
    let job_id = int_of_string job_id in
    Printf.printf "Processing job %d..." job_id;
    Printf.printf "<pre>";
    import_job job_id;
    Printf.printf "</pre>";
    Printf.printf "Finished."

end
