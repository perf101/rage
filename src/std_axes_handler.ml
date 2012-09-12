open Core.Std

let t ~args = object (self)
  inherit Json_handler.t ~args

  method private get_std_xy_choices =
    let machine_field_lst =
      List.tl_exn (Sql.get_col_names ~conn ~tbl:"machines") in
    "branch" :: "build_number" :: "build_tag" ::
    "dom0_memory_static_max" :: "dom0_memory_target" ::
    "cc_restrictions" ::
    machine_field_lst

  method private get_std_x_choices = self#get_std_xy_choices

  method private get_std_y_choices = "result" :: self#get_std_xy_choices

  method private write_body =
    let std_x_axes = self#get_std_x_choices in
    let std_y_axes = self#get_std_y_choices in
    let string_of_axes choices =
      let quoted = List.map ~f:(fun c -> "\"" ^ c ^ "\"") choices in
      sprintf "[%s]" (String.concat ~sep:"," quoted)
    in
    printf "{";
    printf "\"std_x_axes\": %s," (string_of_axes std_x_axes);
    printf "\"std_y_axes\": %s" (string_of_axes std_y_axes);
    printf "}"
end
