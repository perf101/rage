open Async
type t
val connect_pool: conninfo:string -> t
val destroy_pool: t -> unit Deferred.t
val exec_exn: conn:t -> query:string -> Postgresql.result Deferred.t
