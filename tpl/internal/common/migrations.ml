open Migrate_types

module M = Map.Make(String)

let all = ref M.empty

let ocaml_functions = Hashtbl.create 29

let fail_with_error (fname, lineno) msg =
  Printf.eprintf "File %S, line %i:\n%s\n%!" fname lineno msg;
  exit 1

let register_migration id (mig : migration_loc list) =
  all := M.add id mig !all

let register_ocaml_function id func_no (func : Dbi_pg.connection -> unit) =
  let key = (id, func_no) in
  if Hashtbl.mem ocaml_functions key
  then failwith (Printf.sprintf
    "internal error: duplicate ocaml migration function: id = %S, func_no = %i"
    id func_no)
  else
    Hashtbl.add ocaml_functions key func

let get_ocaml_function id func_no =
  let key = (id, func_no) in
  try Hashtbl.find ocaml_functions key
  with Not_found -> failwith (Printf.sprintf
    "Migrations.get_ocaml_function: can't find function by id=%S and number=%i"
    id func_no)

(**************************************************************)

let get () = !all
