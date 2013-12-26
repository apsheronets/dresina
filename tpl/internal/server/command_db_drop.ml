open Proj_common
open Database
open Database_config
open Printf
open Psql

(* todo:
drop database db;
drop owned by user;
drop user user;
*)

let of_opt name opt_v =
  match opt_v with
  | None -> failwith (sprintf
      "in order to use 'db:drop' you should specify ~%s:\"...\" argument \
       in config/database.mlt configuration file"
      name)
  | Some v -> v

let db_drop () : unit =
  let cpi = !!conn_pool_info in
  let ci = cpi.conn_info () in
  let host = of_opt "host" ci#host in
  let dbname = of_opt "dbname" ci#dbname in
  let user = of_opt "user" ci#user in
  let port_opt = ci#port in
  let admin_sql = sprintf
    "drop database \"%s\"; \
     drop owned by \"%s\"; \
     drop user \"%s\";\n"
    dbname
    user
    user
  in
  run_sql ~user:"postgres" ~sql:admin_sql ~host ~port_opt
