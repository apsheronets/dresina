open Proj_common
open Dbi
open Dbi_pg

type conn_pool_info =
  { conn_info : unit -> conn_info
  ; pool : int
  }

let ref_conn_pool_info = ref None

let conn_pool_info = lazy begin
  match !ref_conn_pool_info with
  | None -> failwith "no database connection configured"
  | Some i -> i
end

let register ?host ?port ?dbname ?user ?password
  ?options ?requiressl ?tty
  ?conninfo
  ?(pool = 5)
  ()
 =
  ref_conn_pool_info := Some
    { conn_info = begin fun () -> ((new conn_info
        ?host ?port ?dbname ?user ?password
        ?options ?requiressl ?tty
        ?conninfo
        ()   ) : conn_info)
      end
    ; pool = pool
    }

let pool = lazy begin
  Lwt_pool.create
    (!!conn_pool_info).pool
    (fun () ->
       Lwt_preemptive.detach
         (fun () -> new connection ((!!conn_pool_info).conn_info ()))
         ()
    )
end

let with_connection f =
  Lwt_pool.use !!pool
    (fun conn ->
       Lwt_preemptive.detach f conn
    )
