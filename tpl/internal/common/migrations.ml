open Migrate_types

module M = Map.Make(String)

let all = ref M.empty

let fail_with_error (fname, lineno) msg =
  Printf.eprintf "File %S, line %i:\n%s\n%!" fname lineno msg;
  exit 1

let register_migration id (mig : migration_loc list) =
  all := M.add id mig !all

(**************************************************************)

let get () = !all
