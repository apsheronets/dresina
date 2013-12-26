open Cd_All
open Strings.Latin1
open Migrate_types
module M = Migrations.M
open Schema_types

(**********************************)

let create_type_desc n ml =
  { ty_name = n
  ; ty_ml_name = ml
  ; ty_pg_of_string = None
  ; ty_pg_to_string = None
  ; ty_pg_ddl = None
  ; ty_parent_type = None
  }

let check_type_desc_completeness ty =
  let error_attrs = ref [] in
  let error aname = error_attrs := aname :: !error_attrs in
  let rec inh_chain ty =
    ty.ty_name ::
    match ty.ty_parent_type with
    | None -> []
    | Some par -> inh_chain par
  in
  let rec check_attr aname agetter ty =
    match agetter ty, ty.ty_parent_type with
    | Some _, _ -> ()
    | None, Some par -> check_attr aname agetter par
    | None, None -> error aname
  in
  check_attr "pg_of_string" (fun ty -> ty.ty_pg_of_string) ty;
  check_attr "pg_to_string" (fun ty -> ty.ty_pg_to_string) ty;
  check_attr "pg_ddl" (fun ty -> ty.ty_pg_ddl) ty;
  if !error_attrs = []
  then None
  else Some begin
    Printf.sprintf "undefined attributes: %s (inheritance chain: %s)"
      (String.concat ", " !error_attrs)
      (String.concat " <- " & inh_chain ty)
  end

exception Error of (loc * string * string)

let apply_migration
{ s_types = types ; s_tables = tables }
mig_id
{ ml_item = mi ; ml_loc = loc } =
  let error fmt =
    Printf.ksprintf (fun s -> raise (Error (loc, mig_id, s))) fmt
  in
  let no_table tname = error "table %S doesn't exist" tname
  and no_type tname = error "type %S doesn't exist" tname in
  let check_table_not_exists tname =
    if Hashtbl.mem tables tname
    then error "table %S already exists" tname
    else ()
  and check_table_exists tname =
    if Hashtbl.mem tables tname
    then ()
    else no_table tname
  and check_type_not_exists tname =
    if Hashtbl.mem types tname
    then error "type %S already exists" tname
    else ()
(*
  and check_type_exists tname =
    if Hashtbl.mem types tname
    then ()
    else no_type tname
*)
  in
  let string_of_column_kind ckind =
    match ckind with
    | Ck_pk -> "primary key"
    | Ck_attr -> "column"
    | Ck_fk reftable -> Printf.sprintf "reference (to table %S)" reftable
  in
  let check_column_ref_exists
   { cr_table = tname ; cr_column = cname ; cr_kind = ckind } =
    check_table_exists tname;
    let tab = Hashtbl.find tables tname in
    let cols = tab.tab_cols in
    if List.exists
      (fun cdc ->
         if cdc.cdc_name = cname
         then
           if cdc.cdc_kind = ckind
           then true
           else error "table %S: %S expected to be a %s, but it's a %s"
             tname cname
             (string_of_column_kind ckind)
             (string_of_column_kind cdc.cdc_kind)
         else
           false
      ) cols
    then ()
    else error "table %S: %s %S doesn't exist"
      tname
      (string_of_column_kind ckind)
      cname
  in
  let check_cdc
    ( { cdc_name = cdc_name
      ; cdc_nullable = cdc_nullable
      ; cdc_type = cdc_type
      ; cdc_kind = cdc_kind
      } as cdc
    )
     =
      if not (Hashtbl.mem types cdc_type)
      then error "column %S has unknown type %S" cdc_name cdc_type
      else if cdc_name = "id"
        && (cdc_nullable || cdc_type <> "id" || cdc_kind <> Ck_pk)
      then error "primary key (\"id\" column) must have type \"id\" \
                  and must be not-nullable"
      else
      let ty = Hashtbl.find types cdc_type in
      match check_type_desc_completeness ty with
      | None -> cdc
      | Some err_msg ->
          error "definition of type %S is not complete: %s" cdc_type err_msg
  in
  let check_col_def
    { cd_name = cd_name ; cd_type = cd_type ; cd_nullable = cd_nullable
    ; cd_kind = cd_kind
    }
    =
      check_cdc
      { cdc_name = cd_name
      ; cdc_nullable = cd_nullable
      ; cdc_type = cd_type
      ; cdc_kind = cd_kind
      }
  and check_cols_uniq cdc_list =
    cdc_list |> List.map (fun d -> (d.cdc_name, d)) |>
    List.group_pairs ~fst_eq:String.eq |>
    List.map_filter
      (fun (name, cols) ->
         if List.length cols <> 1
         then Some name
         else None
      ) |>
    (fun dupes ->
       if dupes <> []
       then error "duplicate column names: %s" (String.concat ", " dupes)
       else cdc_list
    )
  and get_table tname =
    try Hashtbl.find tables tname
    with Not_found -> no_table tname
  and get_type tname =
    try Hashtbl.find types tname
    with Not_found -> no_type tname
  and check_column_ref_can_modify
    { cr_table = tname ; cr_column = cname ; cr_kind = ckind }
    action =
      if cname = "id" || ckind = Ck_pk
      then error "table %S: can't %s column %S" tname action cname
      else ()
  in
  match mi with
  | Create_table { td_name = name ; td_columns = cols } ->
      check_table_not_exists name;
      let cols =
        { cd_name = "id"; cd_type = "id"; cd_nullable = false
        ; cd_kind = Ck_pk
        } :: cols
      in
      Hashtbl.add tables name
        { tab_name = name
        ; tab_cols = check_cols_uniq & List.map check_col_def cols
        ; tab_indexes = []
        }
  | Drop_table tname ->
      check_table_exists tname;
      Hashtbl.remove tables tname
  | Add_column (tname, column_def) ->
      let tab = get_table tname in
      let cols = check_cols_uniq (tab.tab_cols @ [check_col_def column_def]) in
      tab.tab_cols <- cols
  | Drop_column
    ({ cr_table = tname; cr_column = cname; cr_kind = ckind } as cr) ->
      check_column_ref_exists cr;
      check_column_ref_can_modify cr "drop";
      let tab = get_table tname in
      let cols = List.filter
        (fun cdc -> not (cdc.cdc_name = cname && cdc.cdc_kind = ckind))
        tab.tab_cols in
      tab.tab_cols <- cols
  | Create_index (tname, index_expr) ->
      let tab = get_table tname in
      let indexes = tab.tab_indexes in
      if List.exists ( ( = ) index_expr ) indexes
      then error "table %S: index on %S already exists" tname index_expr
      else tab.tab_indexes <- index_expr :: indexes
  | Drop_index (tname, index_expr) ->
      let tab = get_table tname in
      let indexes = tab.tab_indexes in
      if List.exists ( ( = ) index_expr ) indexes
      then tab.tab_indexes <- List.filter ( ( <> ) index_expr ) indexes
      else error "table %S: index on %S doesn't exist" tname index_expr
  | Rename_table (toldname, tnewname) ->
      let tab = get_table toldname in
      if toldname = tnewname
      then error
        "can't rename table %S to %S, names are equal" toldname tnewname
      else
      Hashtbl.remove tables toldname;
      let tab = { tab with tab_name = tnewname } in
      Hashtbl.add tables tnewname tab
  | Rename_column
      ( ({ cr_table = tname ; cr_column = oldcname ; cr_kind = ckind }
          as old_column_ref)
      , new_column_name
      ) ->
      check_column_ref_exists old_column_ref;
      check_column_ref_can_modify old_column_ref "rename";
      if oldcname = new_column_name
      then error "table %S: can't rename column %S to %S, names are equal"
        tname oldcname new_column_name
      else
      let tab = get_table tname in
      let cols = List.map
        (fun cdc ->
           if cdc.cdc_name = oldcname && cdc.cdc_kind = ckind
           then { cdc with cdc_name = new_column_name }
           else cdc
        )
        tab.tab_cols
      in
      tab.tab_cols <- check_cols_uniq cols
  | Modify_column
      ( ({ cr_table = tname ; cr_column = cname ; cr_kind = ckind }
           as column_ref
        )
      , col_mod
      ) ->
      let tab = get_table tname in
      check_column_ref_exists column_ref;
      check_column_ref_can_modify column_ref "modify";
      tab.tab_cols <-
        List.map
          (fun cdc ->
             if cdc.cdc_name <> cname || cdc.cdc_kind <> ckind
             then cdc
             else
               check_cdc &
               match col_mod with
               | Cm_set_nullable new_nullable ->
                   if cdc.cdc_nullable = new_nullable
                   then error "table %S: column %S already has nullable=%b"
                     tname cname cdc.cdc_nullable
                   else { cdc with cdc_nullable = new_nullable }
               | Cm_set_type new_type ->
                   if cdc.cdc_type = new_type
                   then error "table %S: column %S already has type=%S"
                     tname cname cdc.cdc_type
                   else
                     match ckind with
                     | Ck_pk -> assert false
                     | Ck_attr -> { cdc with cdc_type = new_type }
                     | Ck_fk _ -> error
                         "table %S: can't modify type of reference %S"
                         tname cname
          )
          tab.tab_cols
  | Generic _ -> ()
  | Create_type (t, ml) ->
      check_type_not_exists t;
      Hashtbl.add types t (create_type_desc t ml)
  | Inherit_type (child_name, base_name) ->
      check_type_not_exists child_name;
      let base = get_type base_name in
      Hashtbl.add types child_name
        { (create_type_desc child_name base.ty_ml_name) with
          ty_parent_type = Some base
        }
  | Pg_of_string (tname, b) ->
      let t = get_type tname in
      begin match t.ty_pg_of_string with
      | None -> t.ty_pg_of_string <- Some b
      | Some _ -> error "type %S: pg_of_string is already defined" tname
      end
  | Pg_to_string (tname, b) ->
      let t = get_type tname in
      begin match t.ty_pg_to_string with
      | None -> t.ty_pg_to_string <- Some b
      | Some _ -> error "type %S: pg_to_string is already defined" tname
      end
  | Pg_ddl (tname, b) ->
      let t = get_type tname in
      begin match t.ty_pg_ddl with
      | None -> t.ty_pg_ddl <- Some b
      | Some _ -> error "type %S: pg_ddl is already defined" tname
      end

(**********************************)

let () =
  if Array.length Sys.argv <> 2
  then failwith (Printf.sprintf
    "%s: must be used with command line argument 'filename of schema.bin'"
    Sys.argv.(0)
  )
  else
    ()

let schema_bin_fname = Sys.argv.(1)

let schema =
  { s_types = Hashtbl.create 67
  ; s_tables = Hashtbl.create 67
  }

let make_schema () =
  Register_all_migrations.register ();
  let m = Migrations.get () in
  M.iter
    (fun id mig_list ->
      Printf.printf "registered migration id %S\n%!" id;
      List.iter (apply_migration schema id) mig_list
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
                (Printf.sprintf "Error applying migration %S: %s" id txt)
          | e ->
              Printf.eprintf "Error applying migrations: %s\n%!"
                (Printexc.to_string e)
        in
        exit 1
      end
