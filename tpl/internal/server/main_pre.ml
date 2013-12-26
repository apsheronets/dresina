open Am_All
open Amall_types
open Proj_common

let (my_listener, http_root, _ws_root) = S.listener_create (`Inet_any 4000)

let http_root_endpoint =
  ( http_root
  , `Service ([], "")
  )

open Amall_http

exception No_route

let () = Database_config.register ()

(*
let () = Lwt.ignore_result &
  (IO.catch (fun () ->
   Database.with_connection & fun c ->
     let () = Dbi_pg.cmd_ok & c#execute "create table asd (i int)" in ()
   )
   (fun e -> Printf.eprintf "asd: %s\n%!" (Printexc.to_string e); IO.return ())
  )
*)
