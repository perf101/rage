open Core.Std
open Utils

(** Combines GET and POST parameters. *)
let get_params_of_request () =
  let get_req = Sys.getenv_exn "QUERY_STRING" in
  let post_req = In_channel.input_all In_channel.stdin in
  let req = get_req ^ (if post_req = "" then "" else "&" ^ post_req) in
  let parts = String.split req ~on:'&' in
  let opt_split part =
    Option.value ~default:(part, "") (String.lsplit2 part ~on:'=') in
  let remove_brackets (n, v) =
    Option.value ~default:n (String.chop_suffix n ~suffix:"%5B%5D"), v in
  List.stable_dedup (List.map ~f:(Fn.compose remove_brackets opt_split) parts)

let place_of_params ~params =
  let open List.Assoc in
  let open Place in
  match find params "p" with Some p -> Place.of_string p | None ->
  match find params "t" with Some _ -> RedirectTinyUrl | None ->
  match find params "som" with Some _ -> SomPage | None ->
  Default

let handle_request () =
  let start_time = Unix.gettimeofday () in
  let params = get_params_of_request () in
  let place = place_of_params ~params in
  let conn = new Postgresql.connection ~conninfo:Sys.argv.(1) () in
  let args = let open Handler in {conn; params} in
  let open Place in
  let handler = begin match place with
    | Default -> Default_handler.t
    | CreateTinyUrl -> Create_tiny_url_handler.t
    | RedirectTinyUrl -> Redirect_tiny_url_handler.t
    | SomPage -> Som_page_handler.t
    | SomData -> Som_data_handler.t
    | SomIndex -> Som_index_handler.t
    | Soms -> Soms_handler.t
    | SomsByTc -> Javascript_only_handler.t
    | StdAxes -> Std_axes_handler.t
    | Briefs  -> Briefs_handler.t
    | Brief -> Brief_handler.t
    | ImportPage -> Import_page_handler.t
    | ImportJobs -> Import_jobs_handler.t
  end in (handler ~args)#handle;
  conn#finish;
  let elapsed_time = Unix.gettimeofday () -. start_time in
  debug (sprintf "==========> '%s': %fs." (Place.string_of place) elapsed_time)

let bind_modules () =
  Sql.debug_fn := None; (* Some debug; *)
  Sql.show_sql := true;
  Sql.time_queries := true;
  Sql.ignore_limit_0 := true;
  Sql.mode := Sql.Live

let _ =
  bind_modules ();
  try handle_request ()
  with Failure msg -> Printexc.print_backtrace stderr; printf "<b>%s</b>" msg
