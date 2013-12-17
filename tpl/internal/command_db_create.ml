open Proj_common
open Database
open Database_config
open Printf

let of_opt name opt_v =
  match opt_v with
  | None -> failwith (sprintf
      "in order to use 'db:create' you should specify ~%s:\"...\" argument \
       in config/database.mlt configuration file"
      name)
  | Some v -> v

let db_create () =
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
  let psql_cmd ~user = sprintf
    "psql -h %s -U %s -d postgres %s"
    (Filename.quote host)
    (Filename.quote user)
    (match port_opt with
     | None -> ""
     | Some p -> "-p " ^ Filename.quote p
    )
  in
  Printf.printf
    "NOTE: if you are running this command not under 'postgres' OS user,\n\
    \      you will be asked for 'postgres' database user password.\n%!";
  let run_sql ~user ~sql =
    let psql_ch = Unix.open_process_out (psql_cmd ~user) in
    output_string psql_ch sql;
    flush psql_ch;
    ignore (Unix.close_process_out psql_ch)
  in
  run_sql ~user:"postgres" ~sql:admin_sql
