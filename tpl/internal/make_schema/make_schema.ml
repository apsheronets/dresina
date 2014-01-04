open Cd_All
open Schema_types
open Apply_migrations
module Cg = Codegen

(**********************************)

let schema_bin_fname = "db/schema.bin"
and schema_code_fname = "db/schema_code.ml"

let schema = create_empty_schema ()

let hashtbl_map_to_list f h =
  Hashtbl.fold
    (fun k v acc -> (f k v) :: acc)
    h
    []

let emit_schema_code () =
  Filew.spit_bin schema_code_fname begin
    "open Migrate_types\n\
     let ddl_of_type : (string, column_type_modifier -> string) Hashtbl.t =\n\
    \  Hashtbl.create 67\n"
    ^ Cg.Struc.items
        (hashtbl_map_to_list
           (fun tyname tydesc ->
              let body = get_type_attr (fun ty -> ty.ty_pg_ddl) tydesc in
              Printf.sprintf
                "let () = Hashtbl.add ddl_of_type %S (\n%s\n)\n"
                  tyname body
           )
           schema.s_types
        )
  end

let don't_generate_sql =
  object
    method add_column _ _ = ""
    method create_index ~tname:_ ~index_expr:_ ~iname:_ = ""
    method create_table ~indexes:_ _ _ = ""
    method drop_column _ _ = ""
    method drop_index ~iname:_ = ""
    method drop_table _ = ""
    method modify_column_nullable ~tname:_ ~cname:_ ~nullable:_ = ""
    method modify_column_type ~tname:_ ~cname:_ ~new_type:_ ~new_type_mod:_ =""
    method rename_column ~tname:_ ~oldcname:_ ~newcname:_ = ""
    method rename_table ~toldname:_ ~tnewname:_ = ""
  end

let make_schema () =
  Register_all_migrations.register ();
  let m = Migrations.get () in
  Migrations.M.iter
    (fun id mig_list ->
      Printf.printf "registered migration id %S\n%!" id;
      let (_ : Migrate_types.executable_migration) =
        apply_mig_list don't_generate_sql schema id mig_list in
      ()
    )
    m;
  Tagged_marshal.to_file
    ~tag:Schema_types.marshal_tag schema_bin_fname
    schema;
  emit_schema_code ()

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
