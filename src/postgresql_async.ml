open Core
open Async

(* [in_thread ~name f] runs the blocking function [f] in a worker thread *)
let in_thread ~name f =
  In_thread.run ~name (fun () -> Or_error.try_with ~backtrace:true f)

let connect ~conninfo =
  in_thread ~name:"Postgresql connect" (fun () ->
      new Postgresql.connection ~conninfo ())

let close c =
  in_thread ~name:"Postgresql close connection" (fun () -> c#finish)

let exec_exn ~(conn : Postgresql.connection) ~query =
  in_thread ~name:"Postgresql query" (fun () ->
      (* previous invocation might've left the connection in a bad state *)
      conn#try_reset ; Sql.exec_exn ~conn ~query)

module Lazy_pooled_resource = struct
  type 'a t = 'a Or_error.t Lazy_deferred.t Throttle.t

  (** [create ~acquire ~release ~limit] creates a lazily initialized resource pool.
   * This pool has at most [n] resources, acquired on demand.
   * *)
  let create ~acquire ~release ~limit : 'a t =
    let pool =
      limit
      |> List.init ~f:(fun _ -> Lazy_deferred.create acquire)
      |> Throttle.create_with ~continue_on_error:false
    in
    Throttle.at_kill pool (fun c ->
        match c |> Lazy_deferred.peek_exn |> Option.bind ~f:Or_error.ok with
        | None ->
            return ()
        | Some conn ->
            conn |> release |> Deferred.Or_error.ok_exn) ;
    pool

  let destroy pool = Throttle.kill pool ; Throttle.cleaned pool

  (** [with_ pool ~f] acquires a resource from [pool] and runs [f].
   * If all resources in [pool] are in use then a new one is created,
   * as long as the total number of resources in the pool is below the limit
   * specified at creation time. *)
  let with_ (pool : 'a t) ~f =
    Throttle.enqueue pool (fun conn ->
        Deferred.Or_error.bind (conn |> Lazy_deferred.force_exn) ~f)
end

let cores =
  (Linux_ext.cores |> Result.ok |> Option.value ~default:(fun () -> 1)) ()

type t = Postgresql.connection Lazy_pooled_resource.t

let connect_pool ~conninfo =
  let acquire () = connect ~conninfo in
  let release = close in
  Lazy_pooled_resource.create ~acquire ~release ~limit:cores

let destroy_pool = Lazy_pooled_resource.destroy

let exec_exn ~conn ~query =
  Lazy_pooled_resource.with_ conn ~f:(fun conn -> exec_exn ~conn ~query)
  |> Deferred.Or_error.ok_exn
