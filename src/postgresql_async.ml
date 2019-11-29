open Core
open Async

let connect ~conninfo =
  In_thread.run ~name:"postgresql connect" (new Postgresql.connection ~conninfo)

let exec_exn ~(conn:Postgresql.connection) ~query =
  In_thread.run ~name:"postgresql query" (fun () ->
    Sql.exec_exn ~conn ~query)

let with_pool ~conninfo f =
  let cores = (Linux_ext.cores |> Result.ok |> Option.value ~default:(fun () -> 1)) () in
  Deferred.List.init ~how:`Parallel ~f:(fun _ -> connect ~conninfo) cores >>= fun resources ->
  Monitor.protect
  ~finally:(fun () ->
    List.iter ~f:(fun c -> c#finish) resources;
    return ())
  (fun () ->
    let pool = Throttle.create_with ~continue_on_error:false resources in
    Throttle.at_kill pool (fun c -> c#try_reset; return ());
    let with_conn f = Throttle.enqueue pool (fun conn ->
      f ~exec_exn:(exec_exn ~conn)
      ) in
    Monitor.protect ~finally:(fun () -> Throttle.prior_jobs_done pool)
    (fun () -> f ~with_conn))
