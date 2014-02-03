open Cd_All



type update_generator =
  { ug_buf : Buffer.t
  ; mutable ug_no : int
  ; mutable ug_params_rev : string list
  }

let ug_create tname id_val =
  let buf = Buffer.create 100 in
  Printf.bprintf buf "update \"%s\" set " tname;
  { ug_buf = buf
  ; ug_no = 2
  ; ug_params_rev = [Int64.to_string id_val]
  }

let ug_add ug name v =
  let no = ug.ug_no in
  ug.ug_no <- no + 1;
  let buf = ug.ug_buf in
  if no = 2  (* first field *)
  then ()
  else Buffer.add_string buf ", ";
  Printf.bprintf buf "\"%s\" = $%i" name no;
  ug.ug_params_rev <- v :: ug.ug_params_rev

let ug_exec ug =
  let buf = ug.ug_buf in
  Buffer.add_string buf " where id = $1";
  let sql = Buffer.contents buf
  and params = Array.of_list (List.rev ug.ug_params_rev) in
  (* Printf.eprintf "DBG UPDATE: %s\n%!" sql; *)
  Database.pg_command_ok sql ~params
