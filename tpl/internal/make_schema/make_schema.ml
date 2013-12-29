open Cd_All
open Schema_types
open Apply_migrations

(**********************************)

let schema_bin_fname = "db/schema.bin"

let schema = create_empty_schema ()

let make_schema () =
  Register_all_migrations.register ();
  let m = Migrations.get () in
  Migrations.M.iter
    (fun id mig_list ->
      Printf.printf "registered migration id %S\n%!" id;
      let (_ : Migrate_types.executable_migration) =
        apply_mig_list schema id mig_list in
      ()
    )
    m;
  Tagged_marshal.to_file
    ~tag:Schema_types.marshal_tag schema_bin_fname
    schema

let () =
  try
    make_schema ()
  with
    e ->
      begin
        if Sys.file_exists schema_bin_fname
        then Sys.remove schema_bin_fname
        else ();
        let () =
          match e with
          | Error ((fname, lineno), id, txt) ->
              Codegen.codegen_error fname lineno
                (Printf.sprintf "Error checking migration %S: %s" id txt)
          | e ->
              Printf.eprintf "Error checking migrations: %s\n%!"
                (Printexc.to_string e)
        in
        exit 1
      end
