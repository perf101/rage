open Core.Std
open Utils

let t ~args = object (self)
  inherit Html_handler.t ~args

  method handle =
    let id = int_of_string (self#get_param_exn "id") in
    (* Define a couple of helper functions for cloning rows. *)
    let field_to_tuple ?(origin = false) ~result ~key ?next ~row col v =
      let k = result#fname col in
      let v = match k with
      | k when k = key -> Option.value_map next ~f:string_of_int ~default:v
      | "report_desc" -> v ^ " COPY"
      | _ -> match v with "t" -> "true" | "f" -> "false" | _ -> v
      in
      if k = key && origin || result#getisnull row col then None else Some (k, v)
    in
    let clone_rows ~tbl ~key ~prev ~next =
      let query = sprintf "SELECT * FROM %s WHERE %s = %d" tbl key prev in
      let result = Sql.exec_exn ~conn ~query in
      List.iteri ~f:(fun i row ->
        let tuples = List.filter_mapi row
          ~f:(field_to_tuple ~result ~key ~next ~row:i) in
        Sql.ensure_inserted ~conn ~tbl ~tuples
      ) result#get_all_lst;
    in
    (* Get definition of report to clone. *)
    let query = sprintf "SELECT * FROM reports WHERE report_id = %d" id in
    let result = Sql.exec_exn ~conn ~query in
    let source = result#get_tuple_lst 0 in
    (* Duplicate its entry in "reports" table. *)
    let tuples = List.filter_mapi source
      ~f:(field_to_tuple ~origin:true ~result ~key:"report_id" ~row:0) in
    let id' = Sql.ensure_inserted_get_id ~conn ~tbl:"reports" ~tuples in
    (* Clone corresponding rows in "report_builds". *)
    clone_rows ~tbl:"report_builds" ~key:"report_id" ~prev:id ~next:id';
    (* Find all corresponding plots. *)
    let query = "SELECT plot_id, report_id, graph_number, som_id " ^
      sprintf "FROM report_plots WHERE report_id = %d" id in
    let result = Sql.exec_exn ~conn ~query in
    (* For each plot.. *)
    let process_plot plot =
      (* Get its current ID. *)
      let plot_id = int_of_string (List.hd_exn plot) in
      (* Create a copy of its definition, and get the copy's ID. *)
      let tuples = List.filter_mapi plot
        ~f:(field_to_tuple ~result ~key:"report_id" ~next:id' ~row:0) in
      let tuples = List.filter ~f:(fun (k, _) -> k <> "plot_id") tuples in
      let plot_id' = Sql.ensure_inserted_get_id ~conn ~tbl:"report_plots" ~tuples in
      (* Clone corresponding rows for this plot, using the copy's ID. *)
      let tbls = List.map ~f:((^) "report_plot_")
        ["tc_configs"; "som_configs"; "split_bys"] in
      List.iter tbls ~f:(fun tbl ->
        clone_rows ~tbl ~key:"plot_id" ~prev:plot_id ~next:plot_id')
    in
    List.iter ~f:process_plot result#get_all_lst;
    (* Redirect back to reports' page. *)
    self#javascript_redirect "/?p=reports"
end
