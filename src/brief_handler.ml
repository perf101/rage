open Core
open Async
open Utils

let () = Ssl_threads.init ()
let () =
  Shutdown.at_shutdown (fun () ->
      Curl.global_cleanup();
      return ())

let config_file = Sys.(get_argv ()).(2)

let config =
  In_channel.(with_file config_file ~f:input_lines)
  |> List.map ~f:(fun line -> Scanf.sscanf line "%s@=%s" (fun k v -> (k,v)) )
  |> String.Table.of_alist_exn

let get_config key =
  match String.Table.find config key with
  | None -> debug (sprintf "Fatal error: Could not find config key '%s' in %s" key config_file); raise Not_found
  | Some x -> x

let rage_username = get_config "rage_username"
let rage_password = get_config "rage_password"
let product_version = get_config "product_version"

(* types of the url input arguments *)
type cols_t = (string * string list) list list [@@deriving sexp]
type rows_t = (string * string list) list list [@@deriving sexp]
type base_t = (string * string list) list [@@deriving sexp]
type baseline_t = int [@@deriving sexp]
type ctx_t  = (string * string list) list [@@deriving sexp]
type str_lst_t = string list [@@deriving sexp]
type sort_by_col_t = int [@@deriving sexp]

type result_t = Avg of float | Range of float * float * float

type job_and_value = {job: int; value: string}
let jobs_of_ms = List.map ~f:(fun m -> m.job)
let vals_of_ms = List.map ~f:(fun m -> m.value)

let k_add_rows_from = "add_rows_from"
let k_for = "for"
let k_endfor = "endfor"
let k_deflist = "deflist"

let t ~args = object (self)
  inherit Html_handler.t ~args

  method private write_body =
    let page_start_time = Unix.gettimeofday () in

    let show_jobids = try bool_of_string (List.Assoc.find_exn ~equal:String.equal params "show_jobids") with _ -> false in
    let no_rounding = try bool_of_string (List.Assoc.find_exn ~equal:String.equal params "no_rounding") with _ -> false in

    let progress str = debug str
    in
    (* === input === *)

    let brief_id = try List.Assoc.find_exn ~equal:String.equal params "id" with |_->"" in

    let url_decode url0 = (* todo: find a more complete version in some lib *)
      let rec loop url_in =
        let decode_once_more = Str.string_match (Str.regexp "%25") url_in 0 in
        let url_out = List.fold_left
            [
              ("%20"," ");("%22","\""); ("%28","("); ("%29",")");   (* unescape http params *)
              ("%2B"," ");("%2C",",");
              ("%2F","/");("%3F","?" ); ("%3D","="); ("%26","&");
              ("%25","%");("+"," ");    ("%3E",">"); ("%3C","<");
              ("%3A",":");("&amp;","&");("&quot;","\"");
              ("%2d","-");
              ("&gt;",">");("&lt;","<");
              ("&45;","-");("&plus;","%2b")
            ]
            ~init:url_in
            ~f:(fun acc (f,t)->(Str.global_replace (Str.regexp f) t acc)) (* f->t *)
        in
        (* loop once more if a %25 was found *)
        if decode_once_more then loop url_out else url_out
      in
      loop url0
    in
    let html_encode html = (* todo: find a more complete version in some lib *)
      List.fold_left
        [
          (">","&gt;");("<","&lt;");   (* escape html text *)
        ]
        ~init:html
        ~f:(fun acc (f,t)->(Str.global_replace (Str.regexp f) t acc)) (* f->t *)
    in

    let parse_url args =
      let key k = Str.replace_first (Str.regexp "/\\?") "" k in
      List.map ~f:(fun p ->
          match String.split ~on:'=' p with
          | k::vs -> (key k), (String.concat ~sep:"=" vs)
          | [] -> failwith "k should be present")
        (String.split ~on:'&' (url_decode args))
    in

    (* extra input from urls *)
    let is_digit id =  Str.string_match (Str.regexp "[0-9]+") id 0 in
    let html_of_url url =
      In_thread.run ~name:"Fetch url" (fun () ->
          try
            let conn = Curl.init() and write_buff = Buffer.create 16384 in
            Curl.set_writefunction conn (fun x->Buffer.add_string write_buff x; String.length x);
            Curl.set_url conn url;
            Curl.set_username conn rage_username;
            Curl.set_password conn rage_password;
            Curl.perform conn;
            Curl.cleanup conn;
            Buffer.contents write_buff;
          with _ -> sprintf "error fetching url %s" url)
    in
    let fetch_brief_params_from_url url =
      (* simple fetch using confluence page with brief_params inside the "code block" macro in the page *)
      let%map html = html_of_url url in
      let html = Str.global_replace (Str.regexp "\n") "" html in (*remove newlines from html*)
      let has_match = Str.string_match (Str.regexp ".*<pre class=\"syntaxhighlighter-pre\"[^>]*>\\([^<]+\\)<") html 0 in (*find the "code block" in the page*)
      if not has_match
      then (printf "Error: no '{code}' block found in %s" url; raise Not_found)
      else
        try Str.matched_group 1 html
        with Not_found -> (debug "not found"; raise Not_found)
    in
    let fetch_brief_params_from_db id =
      let query = sprintf "select brief_params from briefs where brief_id='%s'" id in
      let%map r = Postgresql_async.exec_exn_get_all ~conn ~query in r.(0).(0)
    in
    let fetch_suite id branch =
      let url = sprintf "https://code.citrite.net/projects/XRT/repos/xenrt/raw/suites/%s?at=%s" id (Uri.pct_encode branch) in
      debug (sprintf "Fetching from suite %s" url);
      let%map r = html_of_url url in r, url in
    let pattern = Str.regexp "<param>\\([^=]+\\)=\\([^<]+\\)</param>" in
    let include_rex = Str.regexp "<include filename=\"\\([^\"]+\\)\"" in
    let find_matches html rex =
      let rage_str = ref [] in
      let f str = rage_str := (Str.matched_group 1 str) :: !rage_str; "" in
      ignore (Str.global_substitute rex f html);
      List.rev !rage_str
    in
    let rec fetch_parameters_from inc ~branch =
      let b = Buffer.create 80 in
      let lookup var =
        debug ("Lookup include variable: " ^ var);
        match var with
        | "PRODUCT_VERSION" -> product_version
        | other -> "_"
      in
      Caml.Buffer.add_substitute b lookup inc;
      let inc = Buffer.contents b in
      let%bind html, _ = fetch_suite inc branch in
      let%map includes = includes html ~branch in
      let rage_str = ref [] in
      let f str = rage_str := (Str.matched_group 1 str, Str.matched_group 2 str) :: !rage_str; "" in
      ignore (Str.global_substitute pattern f html);
      List.rev !rage_str |> List.append includes
    and includes html ~branch =
      let%map r = find_matches html include_rex
                  |> Deferred.List.concat_map ~how:`Parallel ~f:(fetch_parameters_from ~branch) in
      debug (sprintf "include parameters: %s"
               (List.map ~f:(fun (k,v) -> sprintf "%s=%s" k v) r |> String.concat ~sep:","));
      r
    in
    let fetch_brief_params_from_suite ?(branch="refs/heads/master") id =
      let%bind html, url = fetch_suite id branch in
      let html = Str.global_replace (Str.regexp "\n") "" html in (*remove newlines from html*)
      let find_matches = find_matches html in
      (* Look for <!-- RAGE --> comments and concatenate their contents *)
      let pattern = Str.regexp "<!-- RAGE\\([^>]*\\)-->" in
      let rows = find_matches pattern |> String.concat ~sep:"\n" in
      let%map includes = includes html ~branch in
      let lookup k =
        if String.(uppercase k = k) then
          match List.Assoc.find ~equal:String.equal includes k with
          | Some v -> v
          | None ->
            failwith (Printf.sprintf "Cannot resolve variable '%s' in %s" k url)
        else "$" ^ k
      in
      let b = Buffer.create (String.length rows) in
      Buffer.add_string b "rows=(";
      Caml.Buffer.add_substitute b lookup rows;
      Buffer.add_string b ")";
      Buffer.contents b
    in
    let fetch_brief_params_from id =
      let xs = if is_digit id then fetch_brief_params_from_db id
        else if String.is_prefix id ~prefix:"TC-" then (
          match String.split ~on:'#' id with
          | [id; branch] -> fetch_brief_params_from_suite ~branch id
          | [id] -> fetch_brief_params_from_suite id
          | _ -> failwith (sprintf "unparseable id '%s'" id)
        ) else fetch_brief_params_from_url id
      in
      (*printf "<html>fetch_brief_params_from %s =<br> %s</html>" id xs;*)
      xs
    in
    let title_of_id id =
      if is_digit id then
        let query = sprintf "select brief_desc from briefs where brief_id='%s'" id in
        let%map r = Postgresql_async.exec_exn_get_all ~conn ~query in r.(0).(0)
      else
        return ""
    in

    let get_input_rows_from_id id fn =
      let%bind brief_params_from = fetch_brief_params_from id in
      let args = parse_url brief_params_from in
      let%map _,_input_rows,_,_,_ = fn args in
      _input_rows
    in

    let rec get_input_values args =

      let params_cols=(try url_decode (List.Assoc.find_exn ~equal:String.equal args "cols") with |_-> "") in
      let params_rows=(try url_decode (List.Assoc.find_exn ~equal:String.equal args "rows") with |_-> "") in
      let params_base=(try url_decode (List.Assoc.find_exn ~equal:String.equal args "base") with |_-> "") in
      let params_baseline=(try url_decode (List.Assoc.find_exn ~equal:String.equal args "baseline") with |_-> "") in
      let params_sort_by_col=(try url_decode (List.Assoc.find_exn ~equal:String.equal args "sort_by_col") with |_-> "") in
      let params_add_rows_from=(try url_decode (List.Assoc.find_exn ~equal:String.equal args k_add_rows_from) with |_-> "") in

      let attempt ~f a =
        try f()
        with Of_sexp_error (exn,t)-> (
            let e = Exn.to_string exn in
            printf "\n<p>syntax error in %s: %s when parsing the substring \"%s\"</p>\n" a e (Sexp.to_string t);
            raise exn
          )
      in

      (* eg.: input_cols_sexp="(((machine_name(xrtuk-08-02 xrtuk-08-04))(active_session_count(1)))((machine_name(xrtuk-08-02 xrtuk-08-04)))((machine_name(xrtuk-08-02 xrtuk-08-04))(active_session_count(2 3)))((machine_name(xrtuk-08-02 xrtuk-08-04))(active_session_count(1 2 3))(soms(288))))" *)
      let input_cols =
        if String.(params_cols <> "") then
          attempt ~f:(fun ()->cols_t_of_sexp (Sexp.of_string params_cols) ) "cols"
        else (*default value *) 
          []
      in
      printf "<input_cols_sexp %s/>\n" (Sexp.to_string (sexp_of_cols_t input_cols));

      printf "<params_rows %s/>\n" (html_encode params_rows);
      let input_rows =
        if String.(params_rows <> "") then
          attempt ~f:(fun ()->rows_t_of_sexp (Sexp.of_string params_rows)) "rows"
        else (*default value *)
          []
      in
      printf "<input_rows_sexp %s/>\n" (html_encode (Sexp.to_string (sexp_of_rows_t input_rows)));
      let%map extra_input_rows_from = (* list of rows_t *)
        let ids = Str.split (Str.regexp ",") params_add_rows_from in
        Deferred.List.map ~how:`Parallel ids ~f:(fun id-> get_input_rows_from_id id get_input_values)
      in
        (*
      printf "<input_extra_rows_sexp %s/>\n" (html_encode (List.fold_left extra_input_rows_from ~init:"" ~f:(fun extra_input_row->(Sexp.to_string (sexp_of_rows_t extra_input_row)))));
      *)
      let input_rows = List.concat (input_rows :: extra_input_rows_from) in
      printf "<input_rows_sexp %s/>\n" (html_encode (Sexp.to_string (sexp_of_rows_t input_rows)));

      (* base context is used to fill any context gap not expressed in row and column contexts
         eg. [("build_number",[44543;55432]);("job_id",[1000;4000]);("number_of_cpus",[1]);...]
         -- append (OR) the results of each element in the list
         TODO: is base context restrictive or conjuntive, ie does it restrict possible contexts in 
         the cells or does it contribute to them with lower-priority than rows and col contexts?
         TODO: use intersection between base_context and input_cols and input_rows
      *)
      let input_base_context = 
        if String.(params_base <> "") then
          attempt ~f:(fun ()->base_t_of_sexp (Sexp.of_string params_base)) "base" 
        else (*default value *)
          []
      in
      printf "<input_base_sexp %s/>\n" (Sexp.to_string (sexp_of_base_t input_base_context));

      let baseline_col_idx =
        if String.(params_baseline <> "") then
          attempt ~f:(fun ()->baseline_t_of_sexp (Sexp.of_string params_baseline)) "baseline"
        else (*default value *)
          0
      in
      printf "<input_baseline_col_sexp %s/>\n" (Sexp.to_string (sexp_of_baseline_t baseline_col_idx));

      let sort_by_col =
        if String.(params_sort_by_col <> "") then
          Some (attempt ~f:(fun ()->sort_by_col_t_of_sexp (Sexp.of_string (String.capitalize params_sort_by_col))) "sort_by_col")
        else (*default value *)
          None
      in
      (input_cols, input_rows, input_base_context, baseline_col_idx, sort_by_col)
    in

    let%bind args =
      if String.(brief_id = "") then return params
      else
        let replace params default_params=
          List.fold_left (* if params present, use it preferrably over the default params *)
            (parse_url default_params)
            ~init:[]
            ~f:(fun acc (k,v)->match List.find params ~f:(fun (ko,_)->String.(k=ko)) with|None->(k,v)::acc|Some o->o::acc)
        in
        let%map brief_params = fetch_brief_params_from brief_id in
        List.fold_left params ~init:(replace params brief_params) ~f:(fun acc (k,v)->
            match List.find acc ~f:(fun (ka,_)->String.(k=ka)) with
            |None->(k,v)::acc (* if params contains a k not in the db, add this k to args *)
            |Some _->acc
          )
    in

    (* === process === *)
    let%bind input_cols, input_rows, input_base_context, baseline_col_idx, sort_by_col =
      get_input_values args
    in

    progress "<p>Progress...:";

    (* Sanity check input arguments *)
    if baseline_col_idx >= List.length input_cols then
      failwith (sprintf "Baseline column is %d but there are only %d columns" baseline_col_idx (List.length input_cols));
    begin
      match sort_by_col with
      | None -> ()
      | Some sort_by_col ->
        if sort_by_col >= List.length input_cols then
          failwith (sprintf "Sort-by column is %d but there are only %d columns" sort_by_col (List.length input_cols));
    end;

    let soms_of_tc tc_fqn =
      let query = sprintf "select som_id from soms where tc_fqn='%s'" tc_fqn in
      let%map a = Postgresql_async.exec_exn_get_all ~conn ~query in
      Array.to_list (Array.map a ~f:(fun x->x.(0)))
    in
    let soms_of_tc = Deferred.Memo.general (module String) soms_of_tc in
    let rec_of_som som_id =
      let query = sprintf "select som_name,tc_fqn,more_is_better,units,positive from soms where som_id='%s'" som_id in
      let%map r = Postgresql_async.exec_exn_get_all ~conn ~query in r.(0)
    in
    let rec_of_som = Deferred.Memo.general (module String) rec_of_som in
    let rec_of_som_id_n som_id n =
      let%map r = unstage(rec_of_som) som_id in r.(n) in
    let name_of_som som_id = rec_of_som_id_n som_id 0 in
    let tc_of_som som_id   = rec_of_som_id_n som_id 1 in
    let more_is_better_of_som som_id = rec_of_som_id_n som_id 2 in
    let unit_of_som som_id = rec_of_som_id_n som_id 3 in
    let has_table table_name =
      let query = sprintf "select table_name from information_schema.tables where table_schema='public' and table_name='%s'" table_name in
      let%map a = Postgresql_async.exec_exn_get_all ~conn ~query in
      not @@ List.is_empty (Array.to_list a)
    in
    let has_table = Deferred.Memo.general (module String) has_table in
    let columns_of_table table_name =
      let query = sprintf "select column_name from information_schema.columns where table_name='%s'" table_name in
      let%map a = Postgresql_async.exec_exn_get_all ~conn ~query in
      Array.to_list (Array.map a ~f:(fun x->x.(0)))
    in
    let columns_of_table = Deferred.Memo.general (module String) columns_of_table in
    let contexts_of_som_id som_id =
      let%map cols = unstage(columns_of_table) (sprintf "som_config_%s" som_id) in
      (List.filter cols
         ~f:(fun e->String.(e<>"som_config_id"))
      )
    in
    let contexts_of_tc_fqn tc_fqn =
      let%map cols = unstage(columns_of_table) (sprintf "tc_config_%s" tc_fqn) in
      (List.filter cols
         ~f:(fun e->String.(e<>"tc_config_id"))
      )
    in
    let url_of_t t =
      let query = sprintf "select url from tiny_urls where key=%s" t in
      let%map r = Postgresql_async.exec_exn_get_all ~conn ~query in r.(0).(0)
    in
    let%bind contexts_of_tc =
      let%map cols = unstage(columns_of_table) "tc_config" in
      (List.filter cols
         ~f:(fun e->not (List.mem ~equal:String.equal ["tc_fqn";"tc_config_id";"machine_id"] e))
      )
    and contexts_of_machine =
      let%map cols = unstage(columns_of_table) "machines" in
      List.filter cols
        ~f:(fun e->String.(e<>"machine_id"))
    and contexts_of_build =
      let%map cols = unstage(columns_of_table) "builds" in
      List.filter cols
        ~f:(fun e->String.(e<>"build_id")) in
    let values_of cs ~at:cs_f = List.filter cs ~f:(fun (k,v)->List.mem ~equal:String.equal cs_f k) in

    (*
    let latest_build_of_branch branch =
      let query = sprintf "select max(build_number) from builds where branch='%s'" branch in
(Postgresql_async.exec_exn_get_all ~conn ~query).(0).(0)
in
*)
    let builds_of_branch branch =
      let query = sprintf "select distinct build_number from builds where branch='%s' order by build_number desc" branch in
      let%map a = Postgresql_async.exec_exn_get_all ~conn ~query in
      List.map (Array.to_list a) ~f:(fun x->x.(0))
    in
    (*  this query is better than builds_of_branch but it is too slow so cannot be used 
        let latest_build_in_branch branch =
        let query = sprintf "select max(build_number) from builds,measurements,jobs where branch='%s' and measurements.job_id = jobs.job_id and jobs.build_id = builds.build_id" branch in
        (Postgresql_async.exec_exn_get_all ~conn ~query).(0).(0)
        in
    *)
    (*TODO: touch each element of the context when it is used; if an element is not used at the end of this function,
        then raise an error indicating that probably there's a typo in the context element
    *) 
    let measurements_of_cell context = 
      let get e ctx = List.Assoc.find ~equal:String.equal ctx e in
      let measurements_of_som som_id =
        let%bind has_table_som_id = unstage(has_table) (sprintf "som_config_%s" som_id)
        and tc_fqn = tc_of_som som_id
        and contexts_of_this_som_id = contexts_of_som_id som_id in
        let%bind contexts_of_this_tc_fqn = contexts_of_tc_fqn tc_fqn in
        let query = "select sj.job_id, m.result from measurements_2 m join soms_jobs sj on m.som_job_id=sj.id "
                    ^(sprintf "join tc_config_%s on m.tc_config_id=tc_config_%s.tc_config_id " tc_fqn tc_fqn)
                    ^(if has_table_som_id then
                        (sprintf "join som_config_%s on m.som_config_id=som_config_%s.som_config_id " som_id som_id)
                      else ""
                     )
                    ^"join tc_config on sj.job_id=tc_config.job_id "
                    ^"join machines on tc_config.machine_id=machines.machine_id "
                    ^"join jobs on tc_config.job_id=jobs.job_id "
                    ^"join builds on jobs.build_id=builds.build_id "
                    ^"where "
                    ^(sprintf "sj.som_id=%s " som_id)
                    ^(List.fold_left (values_of context ~at:contexts_of_machine) ~init:"" ~f:(fun acc (k,vs)->
                        match vs with []->acc|_->
                          sprintf "%s and (%s) " acc
                            (List.fold_left vs ~init:"" ~f:(fun acc2 v->
                                 sprintf "%s%smachines.%s='%s' " acc2 (match acc2 with |""->""|_->"or ") k v
                               ))
                      ))
                    ^(if has_table_som_id then
                        (List.fold_left (values_of context ~at:contexts_of_this_som_id) ~init:"" ~f:(fun acc (k,vs)->
                             match vs with []->acc|_->
                               sprintf "%s and (%s) " acc 
                                 (List.fold_left vs ~init:"" ~f:(fun acc2 v->
                                      sprintf "%s%ssom_config_%s.%s='%s' " acc2 (match acc2 with |""->""|_->"or ") som_id k v
                                    ))
                           ))
                      else ""
                     )
                    ^(List.fold_left (values_of context ~at:contexts_of_this_tc_fqn) ~init:"" ~f:(fun acc (k,vs)->
                        match vs with []->acc|_->
                          sprintf "%s and (%s) " acc
                            (List.fold_left vs ~init:"" ~f:(fun acc2 v->
                                 sprintf "%s%stc_config_%s.%s='%s' " acc2 (match acc2 with |""->""|_->"or ") tc_fqn k v
                               ))
                      ))
                    ^(List.fold_left (values_of context ~at:(contexts_of_tc)) ~init:"" ~f:(fun acc (k,vs)->
                        match vs with []->acc|_->
                          sprintf "%s and (%s) " acc
                            (List.fold_left vs ~init:"" ~f:(fun acc2 v->
                                 sprintf "%s%stc_config.%s='%s' " acc2 (match acc2 with |""->""|_->"or ") k v
                               ))
                      ))
                    ^(List.fold_left (values_of context ~at:(contexts_of_build)) ~init:"" ~f:(fun acc (k,vs)->
                        match vs with []->acc|_->
                          sprintf "%s and (%s) " acc
                            (List.fold_left vs ~init:"" ~f:(fun acc2 v->
                                 sprintf "%s%sbuilds.%s='%s' " acc2 (match acc2 with |""->""|_->"or ") k v
                               ))
                      ))
        in
        let%map a = Postgresql_async.exec_exn_get_all ~conn ~query in
        Array.to_list (Array.map a ~f:(fun x->{job=int_of_string x.(0); value=x.(1)}))
      in
      (* add measurements for each one of the soms in the cell *)
      match get "soms" context with
      | Some som ->
        Deferred.List.concat_map ~how:`Parallel ~f:measurements_of_som som
      | None ->
        failwith (sprintf "Could not find 'soms' in context; keys are [%s]" (List.map ~f:fst context |> String.concat ~sep:", "));
    in

    let context_of base row col =
      (* we use intersection to obtain the result when the same context is present in more than one input source *)
      List.fold_left (base @ row @ col) ~init:[] ~f:(fun acc (ck,cv)->
          let x,ys = List.partition_tf ~f:(fun (k,v)->String.(k=ck)) acc in
          match x with
          |(k,v)::[]->(* context already in acc, intersect the values *)
            if String.(k<>ck) then (failwith (sprintf "k=%s <> ck=%s" k ck));
            (k, List.filter cv ~f:(fun x->List.mem ~equal:String.equal v x))::ys
          |[]->(* context not in acc, just add it *)
            (ck,cv)::ys
          |x->(* error *)
            failwith (sprintf "More than one element with the same context")
        )
    in
    let expand_latest_build_of_branch c_kvs =
      let k_branch = "branch" in
      let k_build_number = "build_number" in
      let v_latest_in_branch = "latest_in_branch" in
      match List.find ~f:(fun (k,vs)->String.(k=k_branch)) c_kvs with
      | None -> return [c_kvs]
      | Some (_,branches) ->
        if List.length branches < 1
        then
          return [] (* no branches provided, no results *)
        else
          (* list of all builds in all branches provided *)

          (* this is the most straightforward way of obtaining the max build of a branch but this query is too slow and cannot be used
             let builds_of_branches = [latest_build_in_branch (List.nth_exn branches 0)] in (*TODO: handle >1 branches in context*)
          *)

          let has_v_latest_in_branch =
            List.exists c_kvs ~f:(fun (k,vs) -> String.(k=k_build_number) && List.exists vs ~f:(fun v->String.(v=v_latest_in_branch)))
          in
          (* if 'latest_in_branch' value is present, expand ctx into many ctxs, one for each build; otherwise, return the ctx intact *)
          if not has_v_latest_in_branch then return [c_kvs]
          else (
            (* brute-force way to find the max build with measurements, to work around the slowness in the query in latest_build_in_branch *)
            let%map builds = builds_of_branch (List.nth_exn branches 0) in (*TODO: handle >1 branch in context*)
            let builds_of_branches = List.slice builds 0 (min 100 (List.length builds)) in (* take up to 100 elements in the list *)
            debug (sprintf "builds_of_branches=%s" (List.fold_left ~init:"" builds_of_branches ~f:(fun acc b->acc ^","^b)));

            List.map builds_of_branches ~f:(fun bs->
                List.map c_kvs ~f:(fun (k,vs) ->
                    if String.(k<>k_build_number) then (k,vs)
                    else k,(List.map vs ~f:(fun v->
                        if String.(v<>v_latest_in_branch) then v else bs
                      ))
                  )
              ))
    in
    let c_kvs_of_tiny_url t =
      let%map url = url_of_t t in
      let url = url_decode url in
      debug (sprintf "expanded tiny url t=%s => %s" t url);
      (* parse and add "v_"k=value patterns in url *)
      let items = parse_url url in
      let kv = List.map
          (List.filter items  (*filter special keys*)
             ~f:(fun (k,v)->
                 (* starts with "v_" or "som" *)
                 ( String.(k="som") ||
                   try Str.search_forward (Str.regexp "v_.*") k 0 = 0 with Not_found->false
                 )
                 && (*and doesn't have 'ALL' as a value*)
                 String.(v<>"ALL")
               )
          )
          ~f:(fun (k,v)-> (*apply some mappings to remaining keys and values *)
              (* remove "v_" from beginning of k *)
              let new_key = Str.replace_first (Str.regexp "v_") "" k in
              let new_value = url_decode v in
              ((if String.(new_key="som") then "soms" else new_key), new_value)
            )
      in

      (* map (k_i,v_i) and (k_j,v_j) to (k_i,[v_i,v_j,...]) when k_i=k_j *)
      let kvs =
        let open Sexplib.Std in
        let ks_tbl = Hashtbl.create 128 in
        List.iter kv
          ~f:(fun (k,v)->
              if Hashtbl.mem ks_tbl k
              then Hashtbl.replace ks_tbl k (v::(Hashtbl.find ks_tbl k)) (* add new v to existing k *)
              else Hashtbl.add ks_tbl k [v] (* add initial v to non-existing k *)
            );
        Hashtbl.fold (fun k vs acc->(k,vs)::acc) ks_tbl []
      in
      kvs
    in
    let expand_tiny_urls c_kvs =
      let k_tiny_url = "t" in
      let tiny_url = List.find c_kvs ~f:(fun (k,_) -> String.(k=k_tiny_url)) in
      let x = match tiny_url with
        | None         -> return [c_kvs]
        | Some (_,[t]) ->
          let%map x = c_kvs_of_tiny_url t in
          [List.fold_left
             ~init:c_kvs           (* c_kvs kvs have priority over the ones in c_kvs_of_tiny_url *)
             x                     (* obtain url from tiny_url id, parse it and return a c_kvs *)
             ~f:(fun acc (k,vs)->
                 if List.exists c_kvs ~f:(fun(_k,_)->String.(k=_k))
                 then (*prefer the one already in c_kvs, ie. do not add (k,vs) to acc*)
                   acc
                 else (*(k,vs) not already in c_kvs, add it *)
                   (k,vs)::acc
               )
          ]
        | Some (_,_) ->
          failwith (sprintf "tiny url: only one tiny url value supported for each t")
      in
      x
    in
    let expand ctx = (*expand cell context into all possible context after expanding ctx templates into values*)
      Deferred.List.fold ~init:[ctx]
        ~f:(fun rets expand_fn ->
            Deferred.List.concat_map rets ~how:`Parallel ~f:expand_fn)
        [
          expand_latest_build_of_branch; (* 1. value template: latest_in_branch *)
          (* expand_tiny_urls;*)         (* 2. key template: t -- to use a tiny link value -- already expanded in row *)
          (* ... potentially other expansions in the future... *)
        ]
    in
    let b = input_base_context in
    let cs = input_cols in
    let rec resolve_keywords rows =

      let deflists : (string * string list) list ref = ref [] in
      let substitions : (string * string list) list ref = ref [] in
      let add_to_each x ys = List.map ~f:(fun y -> x::y) ys in
      let rec transform xs =
        match xs with
        | [] -> [[]]
        | (k,vs)::xs -> List.map vs ~f:(fun v -> add_to_each (k,v) (transform xs)) |> List.concat
      in

      (* Expand any variables defined as lists *)
      let apply_definitions row =
        List.map row ~f:(fun (k,vs) ->
            let new_vs = List.map vs ~f:(fun v ->
                match List.Assoc.find ~equal:String.equal!deflists v with
                | None -> [v]
                | Some exp -> exp
              ) |> List.concat in
            (k, new_vs)
          ) in

      let remove_quotes s =
        let quote_re = Str.regexp "'" in
        Str.global_replace quote_re "" s
      in
      let has_quotes s =
        String.contains s '\''
      in

      (* For a row r and current substitions [(x, [0;1]); (y, [a,b])], return [ r[0/x,a/y]; r[0/x,b/y]; r[1/x,a/y]; r[1/x,b/y] ] *)
      let apply_substitions row =
        let all_subs = transform !substitions in
        List.map all_subs ~f:(fun sub ->
            (* First expand any compound variables, e.g. ("a,b", "0,A") into [("a","0"); ("b","A")] *)
            let sub = List.map sub ~f:(fun (k,v) ->
                let k_split = String.split ~on:',' k in
                let v_split = String.split ~on:',' v in
                List.mapi k_split ~f:(fun i k' ->
                    let v' = List.nth_exn v_split i in
                    match has_quotes v' with
                    | true  -> v' |> remove_quotes |> fun v' -> [(k', v')]                       (* inside quotes  ': use as it is *)
                    | false -> v' |> String.split ~on:' ' |>  List.map ~f:(fun v'' -> (k', v'')) (* outside quotes ': spaces delimit items *)
                  ) |> List.concat
              ) |> List.concat in

            progress (sprintf "current substitions: [%s]" (String.concat ~sep:", " (List.map ~f:(fun (k,v) -> sprintf "(%s, %s)" k v) sub)));

            (* Create a modified row applying this set of substitutions *)
            List.map row ~f:(fun (k,vs) ->
                let new_vs = List.map vs ~f:(fun v ->
                    match List.filter sub ~f:(fun (v',_)->String.(v'=v)) |> List.map ~f:(fun (_,v)->v) with
                    | [] -> [v]
                    | sub_vs -> sub_vs
                  ) |> List.concat
                in
                (k, new_vs)
              )
          )
      in

      Deferred.List.fold ~init:[] rows (* expand special row keys *) (*todo: this should also apply to columns *)
        ~f:(fun acc r-> 
            let resolve_keywords_in_row acc r =

              if List.exists r ~f:(fun (k,v)->String.(k="tcs")) then (* expand tcs into soms *)
                let%map r_expanded = Deferred.List.concat_map r 
                    ~f:(fun (k,v)->match k with 
                        | _ when String.(k="tcs") -> (Deferred.List.concat_map v ~f:(fun tc->
                            let%map r = unstage(soms_of_tc) tc in
                            List.map r ~f:(fun som->("soms",[som]))))
                        | _ -> (k,v)::[] |> return
                      )
                in
                let soms,no_soms = List.partition_tf r_expanded ~f:(fun (k,v)->String.(k="soms")) in
                let soms = List.sort soms ~compare:(fun (xk,xv) (yk,yv)->(int_of_string(List.hd_exn xv)) - (int_of_string(List.hd_exn yv))) in
                acc @ (List.map soms ~f:(fun som->[som] @ no_soms))

              else if List.exists r ~f:(fun (k,v)->String.(k="t")) then (* expand tiny links into rows kvs *)
                let%map lst = expand_tiny_urls r in
                List.hd_exn lst :: acc

              else if List.exists r ~f:(fun (k,_)->String.(k=k_add_rows_from)) then (* add rows from other brief ids *)
                let bs = List.filter r ~f:(fun (k,_)->String.(k=k_add_rows_from)) in (* use all references. TODO: what to do with non-references in the same row??? *)
                let%map r = Deferred.List.concat_map bs ~f:(fun (k,vs)-> (* map one r into many potential rows *)
                    Deferred.List.concat_map vs ~f:(fun v->  (* map one vs into many potential rows *)
                        let%bind xs = get_input_rows_from_id v get_input_values in (* resolve each new row recursively if necessary *)
                        resolve_keywords xs
                      )
                  ) in
                acc @ r

              else if List.exists r ~f:(fun (k,_)->String.(k=k_for)) then (* it's a for-loop! *)
                begin
                  let bs = List.filter r ~f:(fun (k,_)->String.(k=k_for)) in
                  List.iter bs ~f:(fun (_,v) ->
                      let key = List.hd_exn v in
                      let values = List.tl_exn v in
                      progress (sprintf "mapping: key '%s' becomes each of [%s]" key (String.concat ~sep:", " values));
                      substitions := (key, values) :: !substitions
                    );
                  return acc
                end

              else if List.exists r ~f:(fun (k,_)->String.(k=k_endfor)) then (* it's the end of a for-loop! *)
                begin
                  let bs =  List.filter r ~f:(fun (k,_)->String.(k=k_endfor)) in
                  List.iter bs ~f:(fun (_,v) ->
                      substitions := match v with
                        | [] ->
                          begin
                            progress ("unmapping unspecified variable");
                            (* just pop the most recent 'for' variable *)
                            match !substitions with
                            | _::tl -> tl
                            | _ -> failwith ("tried to pop (unspecified) variable from empty stack")
                          end
                        | [v] ->
                          begin
                            progress (sprintf "unmapping '%s'" v);
                            match !substitions with
                            | (hk,hvs)::tl -> if String.(hk=v) then tl else failwith (sprintf "tried to pop variable '%s' but top of stack was '%s'" v hk)
                            | _ -> failwith (sprintf "tried to pop variable '%s' from empty stack" v)
                            (* check the most recent 'for' variable has this name and pop it *)
                          end
                        | _ ->
                          failwith "endfor can have either zero or one parameter"
                    );
                  return acc
                end

              else if List.exists r ~f:(fun (k,_)->String.(k=k_deflist)) then (* it's a deflist *)
                begin
                  let bs = List.filter r ~f:(fun (k,_)->String.(k=k_deflist)) in
                  List.iter bs ~f:(fun (_,v) ->
                      let key = List.hd_exn v in
                      let values = List.tl_exn v in
                      progress (sprintf "definition: name '%s' means array [%s]" key (String.concat ~sep:", " values));
                      deflists := List.Assoc.add ~equal:String.equal !deflists key values
                    );
                  return  acc
                end

              else (* nothing to resolve, carry on *)
                return @@ (r |> apply_substitions |> List.map ~f:apply_definitions) @ acc

            in
            resolve_keywords_in_row acc r
          )
    in
    let%bind rs = resolve_keywords input_rows in
    progress (sprintf "table: %d lines: " (List.length rs));
    let ctx_and_measurements_of_1st_cell_with_data expand_f ctx =
      let%bind ctxs = expand_f ctx in
      let measurements_of_cells = Deferred.List.find_map ctxs ~f:(fun c->let%map ms=measurements_of_cell c in if List.is_empty ms then None else (Some (c,ms))) in
      match%map measurements_of_cells with None->ctx,[]|Some (c,ms)->c,ms
    in
    let%bind measurements_of_table =
      let rs_len = List.length rs in
      Deferred.List.mapi ~how:`Parallel rs ~f:(fun i r->
          progress (sprintf "row %d of %d..." i rs_len);
          let%map csr = Deferred.List.map ~how:`Parallel cs ~f:(fun c->
              let%map ctx, ms = ctx_and_measurements_of_1st_cell_with_data expand (context_of b r c) in
              (r, c, ctx,  ms)
            ) in r, csr
        )
    in

    (* === output === *)

    let n_sum xs = List.fold_left ~init:(0,0.) ~f:(fun (n,sum1) x->succ n, sum1 +. (Float.of_string x)) xs in
    let avg xs = let n,sum=n_sum xs in sum /. (float n) in
    let variance xs = (* 2-pass algorithm *)
      let n,sum1 = n_sum xs in
      if n<2
      then 0.0 (* default variance if not enough measurements present to compute it *)
      else
        let mean = sum1 /. (float n) in
        let sum2 = List.fold_left ~init:0. ~f:(fun sum2 x->sum2 +. ((Float.of_string x) -. mean)*.((Float.of_string x) -. mean)) xs in
        sum2 /. (float (n-1))
    in
    let stddev xs = sqrt (variance xs) in
    let is_valid f = (if Float.is_inf f || Float.is_nan f then false else true) in
    let relative_std_error xs =
      let avg = avg xs in let stddev = stddev xs in
      if (is_valid avg) && (is_valid stddev) then
        Float.to_int (stddev /. avg *. 100.)
      else 0
    in
    ignore (relative_std_error []);

    (* round value f to 4 significant digits *)
    let round ?(significant_digits=4) f =
      if no_rounding then
        Float.to_string f, f
      else
       f |> Float.round_significant ~significant_digits |> Float.to_padded_compact_string, f
    in
    let t_95 n =
      (* For infinitely many values a 95% confidence interval is 1.96 * stddev.
       * However we typically have fewer values, so use the table *)
      if n <= 0 then 0.
      else if n < 30 then
        [|12.71 ;4.303 ;3.182 ;2.776 ;2.571 ;2.447 ;2.365 ;2.306 ;2.262 ;2.228 ;2.201 ;2.179 ;2.160
         ;2.145 ;2.131 ;2.120 ;2.110 ;2.101 ;2.093 ;2.086 ;2.080 ;2.074 ;2.069 ;2.064 ;2.060 ;2.056
         ;2.052 ;2.048 ;2.045|].(n-1)
      else if n <= 120 then
        [|2.042; 2.021; 2.009; 2.000; 1.995; 1.990; 1.987; 1.984; 1.982; 1.980|].((n-30)/10)
      else 1.96
    in
    let of_round avg stddev n ~f0 ~f2 =
      let delta = t_95 (n-1) *. stddev in
      let lower = avg -. delta in (* 95% confidence assuming normal distribution *)
      let upper = avg +. delta in
      let rel = 100.0 *. delta in
      if Float.(abs avg < min_value)
      then f0 ()
      else f2 (round lower) (round avg) (round upper) (round ~significant_digits:2 rel) (* 95% confidence *)
    in
    (* pretty print a value f and its stddev *)
    let str_of_round avg stddev n =
      of_round avg stddev n
        ~f0:(fun ()->"0")
        ~f2:(fun (l,_) (a,_) (u,_) (rel,_) ->
            sprintf "%s±%s%% = [%s, %s]" a rel l u)
    in
    let val_of_round avg stddev n =
      of_round avg stddev n
        ~f0:(fun ()->Avg 0.0)
        ~f2:(fun l a u _ ->Range ((snd l),(snd a),(snd u)) )
    in
    let is_green baseline value more_is_better =
      if more_is_better then
        match baseline, value with
        |Avg b, Avg v-> Float.(v>=b)
        |Avg b, Range (vl, va, vu)-> Float.(va>=b)
        |Range (bl, ba, bu), Avg v-> Float.(v>=ba)
        |Range (bl, ba, bu), Range (vl,va,vu)-> Float.(va>=ba)
      else (* less is better *)
        match baseline, value with
        |Avg b, Avg v-> Float.(v<=b)
        |Avg b, Range (vl, va, vu)-> Float.(va<=b)
        |Range (bl, ba, bu), Avg v-> Float.(v<=ba)
        |Range (bl, ba, bu), Range (vl,va,vu)-> Float.(va<=ba)
    in
    let delta baseline value more_is_better =
      match baseline, value with
      |Avg b, Avg v-> v -. b
      |Avg b, Range (vl, va, vu)-> va -. b
      |Range (bl, ba, bu), Avg v-> v -. ba
      |Range (bl, ba, bu), Range (vl,va,vu)-> va -. ba
    in
    let proportion baseline value more_is_better =
      (delta baseline value more_is_better) /.
      (match baseline with
       |Avg b-> Float.abs b
       |Range (bl, ba, bu)-> Float.abs ba)
    in
    (* pretty print a list of values as average and stddev *) 
    let str_stddev_of xs =
      try
        if List.length xs < 1 then "-"
        else str_of_round (avg xs) (stddev xs) (List.length xs)
      with |e-> sprintf "error %s: %s %f %f " (Exn.to_string e) (Sexp.to_string (sexp_of_str_lst_t xs)) (avg xs) (stddev xs)
    in
    let val_stddev_of xs =
      try
        if List.length xs < 1 then Avg 0.0
        else val_of_round (avg xs) (stddev xs) (List.length xs)
      with |_-> Avg (-1000.0)
    in

    let sort_table mt = (* use url option sort_by_col if present *)
      match sort_by_col with
      |None->mt
      |Some compare_col_idx->
        let mt_xs, mt_0s = List.partition_tf mt
            ~f:(fun (r,cs)->
                let _,_,_,cmp_ms=List.nth_exn cs compare_col_idx in
                let _,_,_,base_ms=List.nth_exn cs baseline_col_idx in
                (List.length cmp_ms > 0) && (List.length base_ms > 0)
              )
        in
        List.sort (mt_xs)  (* rows with at least one measurement *)
          ~compare:(fun (r1,cs1) (r2,cs2) ->
              let ms cs =
                let _,_,_,cmp_ms = List.nth_exn cs compare_col_idx in
                let _,_,_,base_ms = List.nth_exn cs baseline_col_idx in
                proportion (val_stddev_of (vals_of_ms base_ms)) (val_stddev_of (vals_of_ms cmp_ms)) None
              in
              let ms1, ms2 = (Float.abs (ms cs1)),(Float.abs (ms cs2)) in
              if Float.(ms1 > ms2) then -1 else if Float.(ms2 > ms1) then 1 else 0 (* decreasing order *)
            ) @ mt_0s (* rows with no measurements stay at the end *)
    in

    (* compute link to rage graph *)
    (* 1. show v_* values for the union of base+row+(each column) *)
    (* 2. ???split by (f_* ) any keys on the columns *)
    (* eg.: http://perf/?som=41&xaxis=numvms&show_dist=on&f_branch=1&v_build_tag=&v_dom0_memory_static_max=752&v_dom0_memory_target=(NULL)&v_cc_restrictions=f&v_memsize=256&v_vmtype=dom0 *)
    let link_ctx_of_row ctxs =
      List.fold_left ctxs ~init:[] ~f:(fun acc (ck,cv)->
          let x,ys = List.partition_tf ~f:(fun (k,v)->String.(k=ck)) acc in
          match x with
          |(k,v)::[]->(* context already in acc, union the values *)
            if String.(k<>ck) then (failwith (sprintf "link: k=%s <> ck=%s" k ck));
            (k, List.dedup_and_sort ~compare:String.compare (cv @ v))::ys
          |[]->(* context not in acc, just add it *)
            (ck,cv)::ys
          |x->(* error *)
            failwith (sprintf "link: More than one element with the same context")
        )
    in
    let link_ctxs = (List.map (sort_table measurements_of_table) ~f:(fun (r,cs)->link_ctx_of_row (List.concat (List.map cs ~f:(fun (_,_,ctx,_)->ctx))))) in
    let link_xaxis = List.dedup_and_sort ~compare:String.compare (List.concat (List.map cs ~f:(fun c-> List.map c ~f:(fun (x,_)->x)))) in
    let link_xaxis = List.filter link_xaxis ~f:(fun x -> String.(x <> "label")) in


    (* writers *)

    let rage_encode url =
      List.fold_left
        [
          (" ","+");   (* escape http params according to what rage expects *)
        ]
        ~init:url
        ~f:(fun acc (f,t)->(Str.global_replace (Str.regexp f) t acc)) (* f->t *)
    in

    let html_writer table =

      let str_of_values vs=List.fold_left vs ~init:"" ~f:(fun acc v->if String.(acc="") then "\""^v^"\"" else acc^", \""^v^"\"") in
      let str_of_ctxs ?(txtonly=false) kvs = 
        List.fold_left kvs ~init:"" ~f:(fun acc (k,v)->
            (sprintf "%s %s=(%s)%s\n" acc k (str_of_values v) (if txtonly then "" else "<br>") )
          )
      in
      let str_desc_of_ctxs kvs =
        Deferred.List.fold kvs ~init:"" ~f:(fun acc (k,vs)->
            if String.(k<>"soms") then return acc else
              let%map lst = Deferred.List.map ~how:`Parallel vs ~f:(fun som->
                  let%map tc = tc_of_som som
                  and u = unit_of_som som
                  and mb = more_is_better_of_som som
                  and name = name_of_som som in
                  sprintf "%s: <b>%s</b> (%s%s)" tc name (if String.(u="") then u else u^", ") (sprintf "%s is better" (if String.(mb="") then "none" else if String.(mb="f") then "less" else "more"))) in
              (sprintf "%s %s<br>\n" acc (String.concat ~sep:"," lst))
          )
      in
      let link ctx =
        (* link *)
        (
          (* rage is not generic enough to receive an arbirary number of soms in a link, pick just the first one *)
          let som_id=match List.find_exn ctx ~f:(fun (k,_)->String.(k="soms")) with |(k,v)->List.hd_exn v in
          (sprintf "<a href='http://%s/?som=%s&show_dist=on%s%s&#brief_report_analysis'>graph</a>" (Utils.server_name ()) som_id
             (* xaxis *)
             (List.fold_left link_xaxis ~init:"" ~f:(fun acc x->sprintf "%s%s" acc (sprintf "&xaxis=%s" x)))
             (* preset values *)
             (List.fold_left ctx ~init:"" ~f:(fun acc (k,vs)->sprintf "%s%s" acc 
                                                 (List.fold_left vs ~init:"" ~f:(fun acc2 v->sprintf "%s&v_%s=%s" acc2 k (rage_encode v))
                                                 )
                                             ))
          ))
      in
      let is_more_is_better ctx =
        match List.find ctx ~f:(fun (k,_)->String.(k="soms")) with
        |None->return None
        |Some (k,_vs)->(
            let rec is_mb acc vs = (match vs with
                |[]-> return @@ if Option.is_none acc then None else acc
                |v::vs->(let%bind mb = more_is_better_of_som v in
                         if String.(mb="") then is_mb acc vs (* ignore more_is_better if not defined in db *)
                         else
                           let mbtf = match mb with m when String.(m="f")->false|_->true in
                           match acc with
                           |None->is_mb (Some mbtf) vs
                           |Some _mbtf->if Bool.(_mbtf=mbtf)
                             then is_mb (Some mbtf) vs  (* more_is_better values agree between soms *)
                             else return None                  (* more_is_better values disagree between soms *)
                        )
              ) in
            is_mb None _vs
          )
      in
      let num_columns = (List.length cs) + 3 in
      let%bind cells = List.map2_exn table link_ctxs ~f:(fun (r,cs) lnkctx ->
          let%map str_desc = str_desc_of_ctxs r
          and lst =
            Deferred.List.mapi ~how:`Parallel cs ~f:(fun i (r,c,ctx,ms)->
                let _,_,_,baseline_ms = List.nth_exn cs baseline_col_idx in
                let debug_r = Sexp.to_string (sexp_of_ctx_t r)
                and debug_c = Sexp.to_string (sexp_of_ctx_t c)
                and context = str_of_ctxs ctx ~txtonly:true
                and debug_ms = Sexp.to_string (sexp_of_str_lst_t (vals_of_ms ms)) in
                let number = List.length ms in
                let number_str = if show_jobids
                  then
                    sprintf "<sub>[%s]</sub>" (String.concat ~sep:"; " (List.map ~f:string_of_int (List.dedup_and_sort ~compare:Int.compare (jobs_of_ms ms))))
                  else
                    sprintf "<sub>(%d)</sub>" number
                in
                let%bind colour = 
                  (if number = 0 || baseline_col_idx = i then return "" else
                     match%map is_more_is_better ctx with
                     |None->""
                     |Some mb->
                       if (List.length baseline_ms) < 1 then "black" else
                       if is_green (val_stddev_of (vals_of_ms baseline_ms)) (val_stddev_of (vals_of_ms ms)) mb then "green" else "red"
                  ) in
                let avg = str_stddev_of (vals_of_ms ms) in
                let%map diff = 
                  (if number = 0 || baseline_col_idx = i || (List.length baseline_ms < 1) then return "" else
                     match%map is_more_is_better ctx with
                     |None->""
                     |Some mb->sprintf "<sub>(%+.0f%%)</sub>" (100.0 *. (proportion (val_stddev_of (vals_of_ms baseline_ms)) (val_stddev_of (vals_of_ms ms)) mb))
                  ) in
                let text = sprintf "<span style='color:%s'>%s <br> %s %s</span>" colour avg number_str diff in
                sprintf "<div onmouseover=\"this.style.backgroundColor='#FC6'\" onmouseout=\"this.style.backgroundColor='white'\" debug_r='%s' debug_c='%s' title='context:\n%s' debug_ms='%s'>%s</div>" debug_r debug_c context debug_ms text
              ) in
          let cells = List.fold_left ~init:"" ~f:(fun acc c_ms->(sprintf "%s <td style='text-align:center;'> %s </td>\n" acc c_ms)) lst in
          sprintf "<tr> <td style='background-color:skyblue; max-width:200px;'>%s</td> <td style='max-width:200px;'>%s</td> <td>%s</td> %s </tr>\n\n" 
            (* row id/title *)
            (str_of_ctxs r)
            (* row description *)
            str_desc
            (* graph link *)
            (link lnkctx)
            (* cells to the right *)
            cells
        ) |> Deferred.List.all in
      let html_table =
        sprintf "<tr> <td style='background-color:papayawhip;' colspan='%d'>%s</td></tr>\n%s%s%s"
          num_columns
          (* print the base context *)
          (str_of_ctxs b)
          (* print the header *)
          (sprintf "<tr> <td><b>id</b></td> <td><b>Description</b></td> <td><b>View</b></td> %s </tr>"
             (List.foldi cs ~init:"" ~f:(fun i acc _ ->
                  sprintf "%s <td><b>%s</b></td>" acc (if i=baseline_col_idx then "Baseline" else (sprintf "Comparison %d" i))
                ))
          )
          (* print the columns *)
          (sprintf "<tr><td></td><td></td><td></td>%s</tr>"
             (List.fold_left ~init:""
                ~f:(fun acc cs->sprintf "%s <td style='background-color:tan;'> %s </td>" acc (str_of_ctxs cs)) cs
             )
          )
          (* print the cells *)
          (String.concat ~sep:"" cells)
      in
      let brief_name = if is_digit brief_id then "jim #"^brief_id else sprintf "from <a href='%s'>%s</a>" brief_id brief_id in
      let%map title = title_of_id brief_id in
      printf "<p>Brief RAGE Report %s: <b>%s</b></p>\n" brief_name title;
      printf "%s" "<ul><li> Numbers reported at 95% confidence level from the data of existing runs\n";
      printf "%s" "<li> (x) indicates number of samples\n";
      printf "%s" "<li> (x%) indicates difference with baseline column\n";
      printf "%s" "<li> avg±rel%% = [lower, upper] indicates the relative uncertainty (rel=100*t(95%,n-1)*stddev/avg) and 95% confidence interval [avg-t(95%,n-1)*stddev, avg+t(95%,n-1)*stddev].</ul><br>";
      printf "<h4 style='margin:5px'>Report Quality</h4>";
      printf "Rows with data in last column: <span style='font-weight:bold' id='report_quality_data_last'></span><br>";
      printf "Rows with data in 2nd-to-last, but not last: <span style='font-weight:bold' id='report_quality_missing_data_last'></span><br><br>";
      printf "<h4 style='margin:5px'>Filtering</h4>";
      printf "<input name='filterEnabled' value='filtered' type='checkbox'>";
      printf "<label for='filterEnabled'>Enable filtering</label>";
      printf "<br>";
      printf "<span>";
      printf "<select name='filterType'> 
                <option value='regressions'>only regressions</option> 
                <option value='all'>all rows</option>
              </select>";
      printf ">=";
      printf "<input name='minRegression' type='number' value='5' style='width:4em'>%%";
      printf "<span id='freeze_frozen' style='padding:0 10px'><input value='Freeze' title='Delete all hidden rows' type='button'></span>";
      printf "</span>";
      printf "<table>%s</table>" html_table;
      let page_finish_time = Unix.gettimeofday () in
      printf "<hr/>\n";
      printf "<p>Report contained <b>%d rows</b> and took <b>%f seconds</b> to prepare</p>"
        (List.length table)
        (page_finish_time -.  page_start_time);
      (* Javascript *)
      printf "<script src='ragebrief.js'></script>";
    in

    html_writer (sort_table measurements_of_table)

end
