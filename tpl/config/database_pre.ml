type context = string option ref

open Cd_All
open Mlt

let connection1b = string_args1 & fun db_kind body ctx ->
  if db_kind <> "pg"
  then failwith "only 'pg' (postgresql) is supported now"
  else
  if !ctx <> None
  then failwith "only one database configuration is possible, \
                 'environments' are not supported now"
  else
  ctx := Some begin Printf.sprintf
    "let register () = Database.register\n%s\n();;\n"
      body
  end
