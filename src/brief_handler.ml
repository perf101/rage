open Core.Std
open Utils
open Sexplib.Std

(* types of the url input arguments *)
type cols_t = (string * string list) list list with sexp
type rows_t = (string * string list) list list with sexp
type base_t = (string * string list) list with sexp
type baseline_t = int with sexp
type ctx_t  = (string * string list) list with sexp
type str_lst_t = string list with sexp

type result_t = Avg of float | Range of float * float * float

let t ~args = object (self)
  inherit Html_handler.t ~args

(* NOTES:

1) SELECTS:

select measurements.result
from measurements
join tc_config_vmsuspendresume on measurements.tc_config_id=tc_config_vmsuspendresume.tc_config_id
join som_config_122 on measurements.som_config_id=som_config_122.som_config_id
join tc_config on measurements.job_id=tc_config.job_id
join machines on tc_config.machine_id=machines.machine_id
where measurements.som_id=122
and tc_config_vmsuspendresume.vmtype='demo'
and tc_config_vmsuspendresume.storage='local-ext'
and tc_config.redo_log=false
and som_config_122.memsize=2048
and machines.machine_type='Dell PowerEdge 2950';
 
In order to construct that query, we needed to initially know that som 122 comes from the “vmsuspendresume” TC, which we can initially find out with:

select tc_fqn from soms where som_id=122;

2) measurements table: job_id, tc_config_id, som_id, som_config_id, result_id, -- result
-> so, in order to obtain a set of results, one or more of the following need to be defined in the measurement select:
- job_id
- tc_config_id
- som_id
- som_config_id
- result_id      (probably not useful to use in the select for the brief)

2a) tc_config table: job_id, tc_fqn, tc_config_id, machine_id, dom0_memory_static_max, dom0_memory_target, cc_restrictions, redo_log, network_backend

2b) soms table: som_id, som_name, tc_fqn, more_is_better, units, positive

2c) builds table: build_id, product, branch, build_number, build_tag

2d) specialized tables:
tc_config_${tc_fqn}
som_config_${som_id}

*)
  method private write_body =

    (* === input === *)

    let brief_id = try List.Assoc.find_exn params "id" with |_->"" in
    let args =
      if brief_id = "" then params
      else
        let brief_params_of_id id =
          let query = sprintf "select brief_params from briefs where brief_id='%s'" id in
          (Sql.exec_exn ~conn ~query)#get_all.(0).(0)
        in
        let replace kvs overrides =
          List.fold_left kvs ~init:[] ~f:(fun acc (k,v)->match List.find overrides ~f:(fun (ko,_)->k=ko) with|None->(k,v)::acc|Some o->o::acc)
        in
        (*printf "%s" (List.fold_left ~init:"" ~f:(fun acc s->acc ^ "---" ^ s) (String.split ~on:'&' (brief_params_of_id brief_id) )); ( "","","","")*)
        (replace 
          (List.map ~f:(fun p->let ls=String.split ~on:'=' p in (List.nth_exn ls 0),(List.nth_exn ls 1)) (String.split ~on:'&' (brief_params_of_id brief_id) ))
          params (* if params present, use it preferrably over the args in the db *)
        )
    in
    let url_decode url = (* todo: find a more complete version in some lib *)
      List.fold_left
         [
           ("%20"," ");("%22","\"");   (* unescape http params *)
         ]
         ~init:url
        ~f:(fun acc (f,t)->(Str.global_replace (Str.regexp f) t acc)) (* f->t *)
    in
    let params_cols=(try url_decode (List.Assoc.find_exn args "cols") with |_-> "") in
    let params_rows=(try url_decode (List.Assoc.find_exn args "rows") with |_-> "") in
    let params_base=(try url_decode (List.Assoc.find_exn args "base") with |_-> "") in
    let params_baseline=(try url_decode (List.Assoc.find_exn args "baseline") with |_-> "") in

    (* eg.: input_cols_sexp="(((machine_name(xrtuk-08-02 xrtuk-08-04))(active_session_count(1)))((machine_name(xrtuk-08-02 xrtuk-08-04)))((machine_name(xrtuk-08-02 xrtuk-08-04))(active_session_count(2 3)))((machine_name(xrtuk-08-02 xrtuk-08-04))(active_session_count(1 2 3))(soms(288))))" *)
    let input_cols =
      if params_cols <> "" then
        cols_t_of_sexp (Sexp.of_string params_cols) 
      else (*default test value *)
    [
      (* column 1 *)
      [ ("machine_name",["xrtuk-08-02";"xrtuk-08-04 05"]); ("active_session_count",["1"]) ];
      (* column 2 *)
      [ ("machine_name",["xrtuk-08-02";"xrtuk-08-04"]); ];
      (* column 3 *)
      [ ("machine_name",["xrtuk-08-02";"xrtuk-08-04"]); ("active_session_count",["2"; "3"]) ];
      (* column 4 *)
      [ ("machine_name",["xrtuk-08-02";"xrtuk-08-04"]); ("active_session_count",["1";"2";"3"]); ("soms",["288"]); ];
    ] in 
    printf "<input_cols_sexp %s/>\n" (Sexp.to_string (sexp_of_cols_t input_cols));

    let input_rows = 
      if params_rows <> "" then
        rows_t_of_sexp (Sexp.of_string params_rows) 
      else (*default test value *)
    [ (* output sorted by appeareance order in this list *)
      (* row 1 *)
(*    [  ("tcs",["lmbench"]); ]; (* = all soms in lmbench *) *)
      (* row 2 *)
      [ ("soms",["293";"288"]); ];
      (* row 3 *)
      [ ("tcs",["network"]); ];  (* = all soms in network *)
      (* row 4 *)
      [ ("soms",["288"]); ("srtype",["nfs"]); ]; (* more than one argument in a row *) 
      (* row 5 *)
      [ ("soms",["288"]); ("srtype",["ext"]); ]; (* more than one argument in a row *) 
      (* row 6 *)
      [ ("soms",["288"]); ("machine_name",["xrtuk-08-02"]); ("srtype",["nfs"]); ]; (* more than one argument in a row *) 
    ] in
    printf "<input_rows_sexp %s/>\n" (Sexp.to_string (sexp_of_rows_t input_rows));


    (* base context is used to fill any context gap not expressed in row and column contexts
       eg. [("build_number",[44543;55432]);("job_id",[1000;4000]);("number_of_cpus",[1]);...]
       -- append (OR) the results of each element in the list
      TODO: is base context restrictive or conjuntive, ie does it restrict possible contexts in 
            the cells or does it contribute to them with lower-priority than rows and col contexts?
      TODO: use intersection between base_context and input_cols and input_rows
     *)
(*
    let base_context = List.split (Str.regexp ",") (List.assoc.find_exn params "base_context") in
*)
    let input_base_context = 
      if params_base <> "" then
        base_t_of_sexp (Sexp.of_string params_base) 
      else (*default test value *)
    [("build_number",["59235"]); ("active_session_count",["1";"2";"3"]); ]
    (*("dom0_memory_static_max",["4096"]);("number_of_cpus",["2"])]*)
    in
    printf "<input_base_sexp %s/>\n" (Sexp.to_string (sexp_of_base_t input_base_context));

    let baseline_col_idx =
       if params_baseline <> "" then
         baseline_t_of_sexp (Sexp.of_string params_baseline)
       else (*default value *)
         0
    in
    printf "<input_baseline_col_sexp %s/>\n" (Sexp.to_string (sexp_of_baseline_t baseline_col_idx));

    (* === process === *)

    let soms_of_tc tc_fqn =
      let query = sprintf "select som_id from soms where tc_fqn='%s'" tc_fqn in
      Array.to_list (Array.map (Sql.exec_exn ~conn ~query)#get_all ~f:(fun x->x.(0)))
    in
    let rec_of_som som_id =
      let query = sprintf "select som_name,tc_fqn,more_is_better,units,positive from soms where som_id='%s'" som_id in
      (Sql.exec_exn ~conn ~query)#get_all.(0)
    in
    let name_of_som som_id = (rec_of_som som_id).(0) in
    let tc_of_som som_id   = (rec_of_som som_id).(1) in
    let more_is_better_of_som som_id = (rec_of_som som_id).(2) in
    let unit_of_som som_id = (rec_of_som som_id).(3) in
    let has_table table_name =
      let query = sprintf "select table_name from information_schema.tables where table_schema='public' and table_name='%s'" table_name in
      (Array.to_list (Sql.exec_exn ~conn ~query)#get_all) <> []
    in
    let columns_of_table table_name =
      let query = sprintf "select column_name from information_schema.columns where table_name='%s'" table_name in
      Array.to_list (Array.map (Sql.exec_exn ~conn ~query)#get_all ~f:(fun x->x.(0)))
    in
    let contexts_of_som_id som_id =
      (List.filter
        (columns_of_table (sprintf "som_config_%s" som_id))
        ~f:(fun e->e<>"som_config_id")
      )
    in
    let contexts_of_tc_fqn tc_fqn =
      (List.filter
        (columns_of_table (sprintf "tc_config_%s" tc_fqn))
        ~f:(fun e->e<>"tc_config_id")
      )
    in
    let contexts_of_tc =
      (List.filter
        (columns_of_table "tc_config")
        ~f:(fun e->not (List.mem e ~set:["tc_fqn";"tc_config_id";"machine_id"]))
      )
    in
    (*
    let all_contexts_of_tc tc_fqn =
      let tc_contexts = 
        (List.filter
          (columns_of_table "tc_config")
          ~f:(fun e->not (List.mem e ["tc_fqn";"tc_config_id";"machine_id"]))
        )@
        (List.filter
          (columns_of_table (sprintf "tc_config_%d" tc_fqn))
          ~f:(fun e->e<>"tc_config_id")
        )
      in
      List.map
        (List.map (soms_of_tc tc_id) ~f:contexts_of_som)
        ~f:(fun som_contexts->tc_contexts @ som_contexts)
    in
    *)
    let contexts_of_machine = List.filter (columns_of_table "machines") ~f:(fun e->e<>"machine_id") in
    let contexts_of_build = List.filter (columns_of_table "builds") ~f:(fun e->e<>"build_id") in
    let values_of cs ~at:cs_f = List.filter cs ~f:(fun (k,v)->List.mem k ~set:cs_f) in
    

    (*TODO: touch each element of the context when it is used; if an element is not used at the end of this function,
            then raise an error indicating that probably there's a typo in the context element
     *) 
    let measurements_of_cell context = 
       let get e ctx = match List.find_exn ctx ~f:(fun (k,v)->e=k) with |k,v->v in
       let measurements_of_som som_id =
         let has_table_som_id som_id = has_table (sprintf "som_config_%s" som_id) in
         let tc_fqn = tc_of_som som_id in 
         let query = "select measurements.result from measurements "
           ^(sprintf "join tc_config_%s on measurements.tc_config_id=tc_config_%s.tc_config_id " tc_fqn tc_fqn)
           ^(if has_table_som_id som_id then
              (sprintf "join som_config_%s on measurements.som_config_id=som_config_%s.som_config_id " som_id som_id)
             else ""
            )
           ^"join tc_config on measurements.job_id=tc_config.job_id "
           ^"join machines on tc_config.machine_id=machines.machine_id "
           ^"join jobs on tc_config.job_id=jobs.job_id "
           ^"join builds on jobs.build_id=builds.build_id "
           ^"where "
           ^(sprintf "measurements.som_id=%s " som_id)
           ^(List.fold_left (values_of context ~at:contexts_of_machine) ~init:"" ~f:(fun acc (k,vs)->
              match vs with []->""|_->
              sprintf "%s and (%s) " acc
              (List.fold_left vs ~init:"" ~f:(fun acc2 v->
                sprintf "%s%smachines.%s='%s' " acc2 (match acc2 with |""->""|_->"or ") k v
              ))
            ))
           ^(if has_table_som_id som_id then
             (List.fold_left (values_of context ~at:(contexts_of_som_id som_id)) ~init:"" ~f:(fun acc (k,vs)->
              match vs with []->""|_->
              sprintf "%s and (%s) " acc 
              (List.fold_left vs ~init:"" ~f:(fun acc2 v->
                sprintf "%s%ssom_config_%s.%s='%s' " acc2 (match acc2 with |""->""|_->"or ") som_id k v
              ))
            ))
             else ""
            )
           ^(List.fold_left (values_of context ~at:(contexts_of_tc_fqn tc_fqn)) ~init:"" ~f:(fun acc (k,vs)->
              match vs with []->""|_->
              sprintf "%s and (%s) " acc
              (List.fold_left vs ~init:"" ~f:(fun acc2 v->
                sprintf "%s%stc_config_%s.%s='%s' " acc2 (match acc2 with |""->""|_->"or ") tc_fqn k v
              ))
            ))
           ^(List.fold_left (values_of context ~at:(contexts_of_tc)) ~init:"" ~f:(fun acc (k,vs)->
              match vs with []->""|_->
              sprintf "%s and (%s) " acc
              (List.fold_left vs ~init:"" ~f:(fun acc2 v->
                sprintf "%s%stc_config.%s='%s' " acc2 (match acc2 with |""->""|_->"or ") k v
              ))
            ))
           ^(List.fold_left (values_of context ~at:(contexts_of_build)) ~init:"" ~f:(fun acc (k,vs)->
              match vs with []->""|_->
              sprintf "%s and (%s) " acc
              (List.fold_left vs ~init:"" ~f:(fun acc2 v->
                sprintf "%s%sbuilds.%s='%s' " acc2 (match acc2 with |""->""|_->"or ") k v
              ))
            ))
          in
          Array.to_list (Array.map (Sql.exec_exn ~conn ~query)#get_all ~f:(fun x->x.(0)))
        in      
        (* add measurements for each one of the soms in the cell *)
        List.concat (List.map ~f:measurements_of_som (get "soms" context))
    in

    let b = input_base_context in
    let cs = input_cols in
    let rs = List.fold_left ~init:[] input_rows (* expand tcs into soms *) (*todo: this should also apply to columns *)
      ~f:(fun acc r-> let r_expanded = List.concat (List.map r 
          ~f:(fun (k,v)->match k with 
            | _ when k="tcs" -> List.concat (List.map v ~f:(fun tc->List.map (soms_of_tc tc) ~f:(fun som->("soms",[som]))))
            | _ -> (k,v)::[] 
          ))
        in
        let soms,no_soms = List.partition r_expanded ~f:(fun (k,v)->k="soms") in
        let soms = List.sort soms ~cmp:(fun (xk,xv) (yk,yv)->(int_of_string(List.hd_exn xv)) - (int_of_string(List.hd_exn yv))) in
        acc @ (List.map soms ~f:(fun som->[som] @ no_soms))
      )
    in
    let context_of base row col =
      (* we use intersection to obtain the result when the same context is present in more than one input source *)
      List.fold_left (base @ row @ col) ~init:[] ~f:(fun acc (ck,cv)->
        let x,ys = List.partition ~f:(fun (k,v)->k=ck) acc in
        match x with
          |(k,v)::[]->(* context already in acc, intersect the values *)
            if k<>ck then (failwith (sprintf "k=%s <> ck=%s" k ck));
            (k, List.filter cv ~f:(fun x->List.mem x ~set:v))::ys
          |[]->(* context not in acc, just add it *)
            (ck,cv)::ys
          |x->(* error *)
            failwith (sprintf "More than one element with the same context")
      )
    in
    let measurements_of_table = 
      List.map rs ~f:(fun r->
        List.map cs ~f:(fun c->
          let ctx = context_of b r c in
          (r, c, ctx,  measurements_of_cell ctx)
        )
      )
    in


    (* compute link to rage graph *)
    (* 1. show v_* values for the union of base+row+(each column) *)
    (* 2. ???split by (f_* ) any keys on the columns *)
    (* eg.: http://perf/?som=41&xaxis=numvms&show_dist=on&f_branch=1&v_build_tag=&v_dom0_memory_static_max=752&v_dom0_memory_target=(NULL)&v_cc_restrictions=f&v_memsize=256&v_vmtype=dom0 *)

    let link_ctx_of_row ctxs =
      List.fold_left ctxs ~init:[] ~f:(fun acc (ck,cv)->
        let x,ys = List.partition ~f:(fun (k,v)->k=ck) acc in
        match x with
          |(k,v)::[]->(* context already in acc, union the values *)
            if k<>ck then (failwith (sprintf "link: k=%s <> ck=%s" k ck));
            (k, List.dedup (cv @ v))::ys
          |[]->(* context not in acc, just add it *)
            (ck,cv)::ys
          |x->(* error *)
            failwith (sprintf "link: More than one element with the same context")
      )
    in
    let link_ctxs = (List.map measurements_of_table ~f:(fun r->link_ctx_of_row (List.concat (List.map r ~f:(fun (_,_,ctx,_)->ctx))))) in
    let link_xaxis = List.dedup (List.concat (List.map cs ~f:(fun c-> List.map c ~f:(fun (x,_)->x)))) in



    (* === output === *)

    let n_sum xs = List.fold_left ~init:(0,0.) ~f:(fun (n,sum1) x->succ n, sum1 +. (float_of_string x)) xs in
    let avg xs = let n,sum=n_sum xs in sum /. (float n) in
    let variance xs = (* 2-pass algorithm *)
      let n,sum1 = n_sum xs in
      if n<2
      then 0.0 (* default variance if not enough measurements present to compute it *)
      else
        let mean = sum1 /. (float n) in
        let sum2 = List.fold_left ~init:0. ~f:(fun sum2 x->sum2 +. ((float_of_string x) -. mean)*.((float_of_string x) -. mean)) xs in
        sum2 /. (float (n-1))
    in
    let stddev xs = sqrt (variance xs) in
    let is_valid f = match classify_float f with |FP_infinite|FP_nan->false |_->true in
    let relative_std_error xs =
      let avg = avg xs in let stddev = stddev xs in
      if (is_valid avg) && (is_valid stddev) then
        int_of_float (stddev /. avg *. 100.)
      else 0
    in
    ignore (relative_std_error []);

    (* round value f to the optimal decimal place according to magnitude of its stddev *)
    let round f stddev =
      if abs_float (stddev /. f) < 0.00000001 (* stddev = 0.0 doesn't work because of rounding errors in the float representation *)
      then (sprintf "%f" f), f
      else
      (* 0. compute magnitude of stddev relative to f *)
      let f_abs = abs_float f in
      let magnitude = (log stddev) /. (log 10.0) in
      let newdotpos = (if is_valid magnitude then int_of_float (if magnitude < 0.0 then floor (magnitude) else (floor magnitude) +. 1.0) else 1) in
      let f_str = sprintf "%f" f_abs in
      let dotpos = (String.index_exn f_str '.') in
      let cutpos = (dotpos - newdotpos) in
      if cutpos < 0
      then ("0",0.0) (* stddev magnitude is larger then value f *)
      else 
        (* 1. round for the computed magnitude of stddev *)
        let dig_from s pos = (String.sub s ~pos:(pos+1) ~len:1) in
        let dig=dig_from f_str cutpos in
        let rounddigit,roundpos = (* round last significant value using the next digit value *) 
          if dig="."
            then (int_of_string (dig_from f_str (cutpos+1)),newdotpos-1)
            else (int_of_string dig,if newdotpos<0 then newdotpos else newdotpos-1)
        in
        let f_rounded = if rounddigit < 5 then f_abs else f_abs +. 10.0 ** (float_of_int roundpos) in
        let f_str_rounded = sprintf "%f" f_rounded in
        (* 2. print only significant digits *)
        let f_result = (
         let f_abs_str_rounded = (if (f_rounded<1.0) 
          then (* print the rounded value up to its last significant digit *)
            String.sub f_str_rounded ~pos:0 ~len:(cutpos+1)
          else (* print the rounded value up to its last significant digit and fill the rest with 0s *)
            sprintf "%s%s" 
              (String.sub f_str_rounded ~pos:0 ~len:(cutpos+1)) 
              (if dotpos-(cutpos+1)>0 then (String.make (dotpos-(cutpos+1)) '0') else "")
          ) in
          (sprintf "%s%s" (if f<0.0 then if f_abs_str_rounded <> "0" then "-" else "" else "") f_abs_str_rounded)
        )
        in
        (
         (*sprintf "f_str=%s stddev=%f magnitude=%f cutpos=%d dotpos=%d newdotpos=%d dig=%s rounddigit=%d roundpos=%d f_rounded=%f f=%f %s" f_str stddev magnitude cutpos dotpos newdotpos dig rounddigit roundpos f_rounded f*)
          f_result, float_of_string f_result
        )

    in
    let of_round avg stddev ~f0 ~f1 ~f2 =
      let lower = avg -. 2.0 *. stddev in (* 2-sigma = 95% confidence assuming normal distribution *)
      let upper = avg +. 2.0 *. stddev in
      if avg < min_float
      then f0 ()
      else if stddev /. avg < 0.05 (* see if the relative std error is <5% *)
        then f1 (round avg stddev)                                           (* 95% confidence *)
        else f2 (round lower stddev) (round avg stddev) (round upper stddev) (* 95% confidence *)
    in
    (* pretty print a value f and its stddev *)
    let str_of_round avg stddev =
      of_round avg stddev
        ~f0:(fun ()->"0")
        ~f1:(fun a->sprintf "%s" (fst a))
        ~f2:(fun l a u->sprintf "[%s, %s, %s]" (fst l) (fst a) (fst u))
    in
    let val_of_round avg stddev =
      of_round avg stddev
        ~f0:(fun ()->Avg 0.0)
        ~f1:(fun a->Avg (snd a))
        ~f2:(fun l a u->Range ((snd l),(snd a),(snd u)) )
    in
    let is_green baseline value more_is_better =
      if more_is_better then
        match baseline, value with
        |Avg b, Avg v-> (v>=b)
        |Avg b, Range (vl, va, vu)-> (va>=b)
        |Range (bl, ba, bu), Avg v-> (v>=ba)
        |Range (bl, ba, bu), Range (vl,va,vu)-> (va>=ba)
     else (* less is better *)
        match baseline, value with
        |Avg b, Avg v-> (v<=b)
        |Avg b, Range (vl, va, vu)-> (va<=b)
        |Range (bl, ba, bu), Avg v-> (v<=ba)
        |Range (bl, ba, bu), Range (vl,va,vu)-> (va<=ba)
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
      |Avg b-> abs_float b
      |Range (bl, ba, bu)-> abs_float ba)
    in
    (* pretty print a list of values as average and stddev *) 
    let str_stddev_of xs =
      try
        if List.length xs < 1 then "-"
        else str_of_round (avg xs) (stddev xs)
      with |_-> sprintf "error: %s %f %f " (Sexp.to_string (sexp_of_str_lst_t xs)) (avg xs) (stddev xs)
    in
    let val_stddev_of xs =
      try
        if List.length xs < 1 then Avg 0.0
        else val_of_round (avg xs) (stddev xs)
      with |_-> Avg (-1000.0)
    in

    let confluence_html_writer table =

      let str_of_values vs=List.fold_left vs ~init:"" ~f:(fun acc v->if acc="" then "\""^v^"\"" else acc^", \""^v^"\"") in
      let str_of_ctxs ?(txtonly=false) kvs = 
        List.fold_left kvs ~init:"" ~f:(fun acc (k,v)->
          (sprintf "%s %s=(%s)%s\n" acc k (str_of_values v) (if txtonly then "" else "<br>") )
        )
      in
      let str_desc_of_ctxs kvs =
        List.fold_left kvs ~init:"" ~f:(fun acc (k,vs)->
          if k<>"soms" then acc else
          (sprintf "%s %s<br>\n" acc (List.fold_left vs ~init:"" ~f:(fun acc2 som->
              let s=sprintf "%s: <b>%s</b> (%s, %s)" (tc_of_som som) (name_of_som som) (unit_of_som som) (sprintf "%s is better" (let mb=more_is_better_of_som som in if mb="" then "none" else if mb="f" then "less" else "more"))  in
              if acc="" then s else acc^","^s
            ))
          )
        )
      in
          let link ctx =
          (* link *)
          (
          (* rage is not generic enough to receive an arbirary number of soms in a link, pick just the first one *)
          let som_id=match List.find_exn ctx ~f:(fun (k,_)->k="soms") with |(k,v)->List.hd_exn v in
          (sprintf "<a href='http://perf/?som=%s&show_dist=on%s%s'>graph</a>" som_id
            (* xaxis *)
            (List.fold_left link_xaxis ~init:"" ~f:(fun acc x->sprintf "%s%s" acc (sprintf "&xaxis=%s" x)))
            (* preset values *)
            (List.fold_left ctx ~init:"" ~f:(fun acc (k,vs)->sprintf "%s%s" acc 
             (List.fold_left vs ~init:"" ~f:(fun acc2 v->sprintf "%s&v_%s=%s" acc2 k v)
             )
            ))
          ))
          in
      let is_more_is_better ctx =
        match List.find ctx ~f:(fun (k,_)->k="soms") with
        |None->None
        |Some (k,_vs)->(
          let rec is_mb acc vs = (match vs with
          |[]->if acc=None then None else acc
          |v::vs->(let mb = more_is_better_of_som v in
            if mb="" then is_mb acc vs (* ignore more_is_better if not defined in db *)
            else
              let mbtf = match mb with m when m="f"->false|_->true in
              match acc with
              |None->is_mb (Some mbtf) vs
              |Some _mbtf->if _mbtf=mbtf
              then is_mb (Some mbtf) vs  (* more_is_better values agree between soms *)
              else None                  (* more_is_better values disagree between soms *)
            )
          ) in
          is_mb None _vs
        )
      in 
      sprintf "<tr> <td style='background-color:papayawhip;'>%s</td></tr>\n%s%s%s"
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
      (List.fold_left ~init:"" ~f:(fun acc r_ms->acc^r_ms)
       (List.map3_exn table rs link_ctxs ~f:(fun cs r lnkctx ->
        sprintf "<tr> <td style='background-color:skyblue; max-width:200px;'>%s</td> <td style='max-width:200px;'>%s</td> <td>%s</td> %s </tr>\n\n" 
          (* row id/title *)
          (str_of_ctxs r)
          (* row description *)
          (str_desc_of_ctxs r)
          (* graph link *)
          (link lnkctx)
          (* cells to the right *)
          (List.fold_left ~init:"" ~f:(fun acc c_ms->(sprintf "%s <td style='text-align:center;'> %s </td>\n" acc c_ms))
            (List.mapi cs ~f:(fun i (r,c,ctx,ms)->
              let _,_,_,baseline_ms = List.nth_exn cs baseline_col_idx in
              sprintf "<div onmouseover=\"this.style.backgroundColor='#FC6'\" onmouseout=\"this.style.backgroundColor='white'\" debug_r='%s' debug_c='%s' title='context:\n%s' debug_ms='%s'>%s</div>"
              (Sexp.to_string (sexp_of_ctx_t r))
              (Sexp.to_string (sexp_of_ctx_t c))
              (str_of_ctxs ctx ~txtonly:true)
              (Sexp.to_string (sexp_of_str_lst_t ms))
              (sprintf "<span style='color:%s'>%s %s %s</span>"
                (if baseline_col_idx = i then "" else
                 match is_more_is_better ctx with
                 |None->""
                 |Some mb->if is_green (val_stddev_of baseline_ms) (val_stddev_of ms) mb then "green" else "red"
                )
                (str_stddev_of ms)
                (sprintf "<sub>(%d)</sub>" (List.length ms))
                (if baseline_col_idx = i then "" else
                 match is_more_is_better ctx with
                 |None->""
                 |Some mb->sprintf "<sub>(%+.0f%%)</sub>" (100.0 *. (proportion (val_stddev_of baseline_ms) (val_stddev_of ms) mb))
                )
              )
            ))
          )
       ))
      ) 
    in
    printf "%s" "<p>Rage Report</p>\n";
    printf "%s" "<p>- Numbers reported at 95% confidence level from the data of existing runs</p>\n";
    printf "%s" "<p>- (x) indicates number of samples</p>\n";
    printf "%s" "<p>- [lower, avg, upper] indicates [avg-2*stddev, avg, avg+2*stddev]. If relative standard error < 5%, only avg is shown.</p>\n";
    printf "<table>%s</table>" (confluence_html_writer measurements_of_table);

(*

select measurements.result
from measurements
join tc_config_vmsuspendresume on measurements.tc_config_id=tc_config_vmsuspendresume.tc_config_id
join som_config_122 on measurements.som_config_id=som_config_122.som_config_id
join tc_config on measurements.job_id=tc_config.job_id
join machines on tc_config.machine_id=machines.machine_id
where measurements.som_id=122
and tc_config_vmsuspendresume.vmtype='demo'
and tc_config_vmsuspendresume.storage='local-ext'
and tc_config.redo_log=false
and som_config_122.memsize=2048
and machines.machine_type='Dell PowerEdge 2950';
 
In order to construct that query, we needed to initially know that som 122 comes from the “vmsuspendresume” TC, which we can initially find out with:

select tc_fqn from soms where som_id=122;




SELECT machines.cpu_model, measurements.result, machines.cpu_model, builds.branch, builds.build_tag, tc_config.dom0_memory_static_max, tc_config.dom0_memory_target, tc_config.cc_restrictions, tc_config_lmbench.memsize, tc_config_lmbench.vmtype FROM measurements, jobs, builds, tc_config, machines, tc_config_lmbench WHERE measurements.tc_config_id=tc_config_lmbench.tc_config_id AND measurements.som_id=41 AND measurements.job_id=jobs.job_id AND jobs.build_id=builds.build_id AND tc_config.job_id=jobs.job_id AND tc_config.tc_fqn='lmbench' AND tc_config.tc_config_id=measurements.tc_config_id AND tc_config.machine_id=machines.machine_id AND tc_config_lmbench.vmtype IN ('dom0') AND tc_config_lmbench.memsize IN (256) AND builds.build_tag IN ('') AND tc_config.dom0_memory_target IS NULL AND tc_config.dom0_memory_static_max IN (752) AND tc_config.cc_restrictions IN ('f'), 


*)

end