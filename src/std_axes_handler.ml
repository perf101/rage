open Core
open Async

let t ~args = object (self)
  inherit Json_handler.t ~args

  method private get_std_xy_choices =
    let%map machine_field_lst =
      let%map r = Postgresql_async.wrap_sql ~conn (Sql.get_col_names ~tbl:"machines") in
      List.tl_exn r in
    "branch" :: "build_number" :: "build_tag" :: "patches_applied" :: "build_is_release" ::
    "dom0_memory_static_max" :: "dom0_memory_target" ::
    "cc_restrictions" :: "redo_log" ::
    machine_field_lst

  method private get_std_x_choices = self#get_std_xy_choices

  method private get_std_y_choices =
    let%map r = self#get_std_xy_choices in
    "result" :: r

  method private write_body =
    let%bind std_x_axes = self#get_std_x_choices in
    let%bind std_y_axes = self#get_std_y_choices in
    let string_of_axes choices =
      let quoted = List.map ~f:(fun c -> "\"" ^ c ^ "\"") choices in
      sprintf "[%s]" (String.concat ~sep:"," quoted)
    in
    printf "{";
    printf "\"std_x_axes\": %s," (string_of_axes std_x_axes);
    printf "\"std_y_axes\": %s" (string_of_axes std_y_axes);
    printf "}";
    return ()
end
