open Proj_common
open Database
open Database_config
open Printf
open Psql

let of_opt name opt_v =
  match opt_v with
  | None -> failwith (sprintf
      "in order to use 'db:create' you should specify ~%s:\"...\" argument \
       in config/database.mlt configuration file"
      name)
  | Some v -> v

let db_create () : unit =
  let cpi = !!conn_pool_info in
  let ci = cpi.conn_info () in
  let host = of_opt "host" ci#host in
  let dbname = of_opt "dbname" ci#dbname in
  let user = of_opt "user" ci#user in
  let password = of_opt "password" ci#password in
  let schema = of_opt "schema" cpi.schema in
  let port_opt = ci#port in
  let admin_sql = sprintf
    "create user \"%s\" with password '%s'; \
     create database \"%s\" owner=\"%s\";\n\
     \\connect \"%s\"\n\
     create schema \"%s\"; \
     grant all privileges on schema \"%s\" to \"%s\";\n"
    user password
    dbname user
    dbname
    schema
    schema user
  in
  run_sql ~user:"postgres" ~sql:admin_sql ~host ~port_opt
