open Core
open Utils

let importer = "/usr/groups/perfeng/bin/importer-xenrt"

(*
 * We accept either:
 *  - jobid from import_page form
 *  - JOBID from XenRT CALLBACK_URL
 *
 * Value can be either '<n>' or '<n0>-<n1>' or '<n0>,<n1>,...'.
 *)

let import_job job_ids =
  debug (sprintf "import_jobs handler importing jobs %s" job_ids);

  (* Sanitise input *)
  if not (Str.string_match (Str.regexp "^[0-9,\\-]*$") job_ids 0) then failwith (sprintf "expected '&lt;n&gt;' or '&lt;n&gt;-&lt;n&gt;' or '&lt;n&gt;,&lt;n&gt;,...'; got '%s'" job_ids);

  let cmd = Printf.sprintf "%s -jobs %s -ignoreseenjobs 2>&1" importer job_ids in
  printf "<!-- %s -->" cmd;
  let ic = Unix.open_process_in cmd in
  In_channel.iter_lines ic ~f:(fun input ->
      printf "%s\n" input;
      eprintf "[import_jobs_handler|%s] %s\n" job_ids input);
  eprintf "[import_jobs_handler|%s] EOF\n" job_ids;
  ignore (Unix.close_process_in ic);
  eprintf "[import_jobs_handler|%s] Finished\n" job_ids

let t ~args = object (self)
  inherit Html_handler.t ~args

  method private write_body =
    let job_ids = try
        self#get_param_exn "jobid"
      with Not_found ->
        self#get_param_exn "JOBID"
    in
    let job_ids = decode_html job_ids in
    Printf.printf "Processing job(s) %s..." job_ids;
    Printf.printf "<pre>";
    import_job job_ids;
    Printf.printf "</pre>";
    Printf.printf "Finished.";
    Async.return ()

end
