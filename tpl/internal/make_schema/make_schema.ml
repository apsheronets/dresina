open Cd_All
open Schema_types
open Apply_migrations
open Codegen.Cg2

(**********************************)

let schema = create_empty_schema ()

let hashtbl_map_to_list f h =
  Hashtbl.fold
    (fun k v acc -> (f k v) :: acc)
    h
    []

let emit_schema_code () =
  Filew.spit_bin schema_code_fname begin
    Implem.to_string &
    [ Struc.open_ ["Migrate_types"]
    ; Struc.expr "ddl_of_type"
        ~typ:(Typ.param
                (Typ.prim ~mod_path:["Hashtbl"] "t")
                [ Typ.prim "string"
                ; Typ.arrow
                    [ Typ.prim "column_type_modifier"
                    ; Typ.prim "string"
                    ]
                ]
             ) &
        Expr.call_mod ["Hashtbl"] "create" [Expr.int 67]
    ]
    @
    (List.flatten &
       hashtbl_map_to_list
         (fun tyname tydesc ->
            let ddl_body = get_type_attr (fun ty -> ty.ty_pg_ddl) tydesc in
            let ml_type = tydesc.ty_ml_name in
            let ctm_typ = Typ.prim "column_type_modifier" in
            [ Struc.let_ Patt.unit &
              Expr.call_mod ["Hashtbl"] "add"
              [ Expr.lid "ddl_of_type"
              ; Expr.string tyname
              ; Expr.of_body ddl_body
              ]
            ; Struc.expr ("pg_" ^ tyname ^ "_of_string")
                ~typ:(Typ.arrow [ctm_typ; Typ.prim "string"; Typ.prim ml_type])
                (Expr.of_body &
                 get_type_attr (fun ty -> ty.ty_pg_of_string) tydesc
                )
            ; Struc.expr ("pg_" ^ tyname ^ "_to_string")
                ~typ:(Typ.arrow [ctm_typ; Typ.prim ml_type; Typ.prim "string"])
                (Expr.of_body &
                 get_type_attr (fun ty -> ty.ty_pg_to_string) tydesc
                )
            ]
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
  Schema_types.Schema_tm.to_file schema_bin_fname schema;
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
              output_codegen_error fname lineno
                (Printf.sprintf "Error checking migration %S: %s" id txt)
          | e ->
              Printf.eprintf "Error checking migrations: %s\n%!"
                (Printexc.to_string e)
        in
        exit 1
      end
