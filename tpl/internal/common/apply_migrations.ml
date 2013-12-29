open Cd_All
open Migrate_types
open Strings.Latin1
open Schema_types
open Proj_common


let create_type_desc n ml =
  { ty_name = n
  ; ty_ml_name = ml
  ; ty_pg_of_string = None
  ; ty_pg_to_string = None
  ; ty_pg_ddl = None
  ; ty_parent_type = None
  }

let ck_ckc_matches ck ckc =
  match ck, ckc with
  | Ck_pk, Ckc_pk
  | Ck_fk _, Ckc_fk _
  | Ck_attr, Ckc_attr -> true
  | Ck_pk, (Ckc_fk _ | Ckc_attr)
  | Ck_fk _, (Ckc_attr | Ckc_pk)
  | Ck_attr, (Ckc_fk _ | Ckc_pk) -> false

let string_of_column_kind ckind =
  match ckind with
  | Ck_pk -> "primary key"
  | Ck_attr -> "column"
  | Ck_fk reftable -> Printf.sprintf "reference (to table %S)" reftable

let string_of_column_kind_checked ckc =
  let ck =
    match ckc with
    | Ckc_pk -> Ck_pk
    | Ckc_attr -> Ck_attr
    | Ckc_fk (_fk_db_name, reftable) -> Ck_fk reftable
  in
    string_of_column_kind ck

let rec get_type_attr_opt agetter ty =
  match agetter ty, ty.ty_parent_type with
  | ((Some _) as aval), _ -> aval
  | None, Some par -> get_type_attr_opt agetter par
  | None, None -> None

let get_type_attr agetter ty =
  match get_type_attr_opt agetter ty with
  | None -> failwith "get_type_attr"
  | Some v -> v

let get_type_ddl ty = get_type_attr (fun ty -> ty.ty_pg_ddl) ty

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
    match get_type_attr_opt agetter ty with
    | Some _ -> ()
    | None -> error aname
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

type app_state =
  { em_rev : executable_migration
  ; prev_up : executable_migration_action option
  }

let finish_state s =
  match s.prev_up with
  | None -> s
  | Some ema ->
      let em_rev = Emi_up ema :: s.em_rev in
      { em_rev = em_rev ; prev_up = None }

let state_add_sql s up down =
  let s = finish_state s in
  { s with em_rev = Emi_up_down ((Ema_sql up), (Ema_sql down)) :: s.em_rev }

let state_add s dir ema =
  match dir with
  | Md_up ->
      let s = finish_state s in
      { s with prev_up = Some ema }
  | Md_down ->
      match s.prev_up with
      | None -> { s with em_rev = Emi_down ema :: s.em_rev }
      | Some up ->
          { prev_up = None
          ; em_rev = Emi_up_down (up, ema) :: s.em_rev
          }

module Gs = Generate_sql

let apply_migration
{ s_types = types ; s_tables = tables }
mig_id
state
{ ml_item = mi ; ml_loc = loc }
 =
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
  let check_column_ref_exists
   { cr_table = tname ; cr_column = cname ; cr_kind = ckind } =
    check_table_exists tname;
    let tab = Hashtbl.find tables tname in
    let cols = tab.tab_cols in
    if List.exists
      (fun cdc ->
         if cdc.cdc_name = cname
         then
           if ck_ckc_matches ckind cdc.cdc_kind
           then true
           else error "table %S: %S expected to be a %s, but it's a %s"
             tname cname
             (string_of_column_kind ckind)
             (string_of_column_kind_checked cdc.cdc_kind)
         else
           false
      ) cols
    then ()
    else error "table %S: %s %S doesn't exist"
      tname
      (string_of_column_kind ckind)
      cname
  in
  let get_type tname =
    try Hashtbl.find types tname
    with Not_found -> no_type tname
  in
  let check_col_type name ty =
    if not (Hashtbl.mem types ty)
    then error "column %S has unknown type %S" name ty
    else ()
  in
  let check_cdc
    ( { cdc_name = cdc_name
      ; cdc_nullable = cdc_nullable
      ; cdc_type = cdc_type
      ; cdc_kind = cdc_kind
      ; cdc_ddl = _
      } as cdc
    )
     =
      if not (Hashtbl.mem types cdc_type)
      then error "column %S has unknown type %S" cdc_name cdc_type
      else if cdc_name = "id"
        && (cdc_nullable || cdc_type <> "id" || cdc_kind <> Ckc_pk)
      then error "primary key (\"id\" column) must have type \"id\" \
                  and must be not-nullable"
      else
      let ty = Hashtbl.find types cdc_type in
      match check_type_desc_completeness ty with
      | None -> cdc
      | Some err_msg ->
          error "definition of type %S is not complete: %s" cdc_type err_msg
  in
  let make_fk_name ~tname ~col_name =
    let base = tname ^ "_" ^ col_name in
    Gs.make_unique_name base "_fk" & fun ident ->
      hashtbl_for_all
        (fun _tname tab ->
           not (List.exists
             (fun col ->
                match col.cdc_kind with
                | Ckc_pk | Ckc_attr -> false
                | Ckc_fk (fk_db_ident, _reftable) -> ident = fk_db_ident
             )
             tab.tab_cols
           )
        )
        tables
  in
  let check_added_col_def
    tname
    { cd_name = cd_name ; cd_type = cd_type ; cd_nullable = cd_nullable
    ; cd_kind = cd_kind
    }
    =
      check_col_type cd_name cd_type;
      let ty = get_type cd_type in
      let cdc_kind =
        match cd_kind with
        | Ck_pk -> Ckc_pk
        | Ck_attr -> Ckc_attr
        | Ck_fk reftable ->
            let fk_db_name = make_fk_name ~tname ~col_name:cd_name in
            Ckc_fk (fk_db_name, reftable)
      in
      check_cdc
      { cdc_name = cd_name
      ; cdc_nullable = cd_nullable
      ; cdc_type = cd_type
      ; cdc_kind = cdc_kind
      ; cdc_ddl = get_type_ddl ty
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
       else ()
    )
  and get_table tname =
    try Hashtbl.find tables tname
    with Not_found -> no_table tname
  and check_column_ref_can_modify
    { cr_table = tname ; cr_column = cname ; cr_kind = ckind }
    action =
      if cname = "id" || ckind = Ck_pk
      then error "table %S: can't %s column %S" tname action cname
      else ()
  and make_index_name ~tname ~index_expr =
    let base = tname ^ "_" ^ index_expr in
    Gs.make_unique_name base "_i" & fun ident ->
      hashtbl_for_all
        (fun _tname tab ->
           not (List.exists
             (fun (i_id, _i_expr) -> i_id = ident)
             tab.tab_indexes
           )
        )
        tables
  and index_expr_eq index_expr = fun (_iname, ie) -> ie = index_expr
  in
  match mi with
  | Mi_generic (dir, ema) ->
      state_add state dir ema
  | Mi_special ms ->
  match ms with
  | Create_table { td_name = tname ; td_columns = cols } ->
      check_table_not_exists tname;
      let tab =
        { tab_name = tname
        ; tab_cols = []
        ; tab_indexes = []
        } in
      Hashtbl.add tables tname tab;
      List.iter
        (fun col ->
           let ccol = check_added_col_def tname col in
           tab.tab_cols <- tab.tab_cols @ [ ccol ]
        )
        cols;
      check_cols_uniq tab.tab_cols;
      state_add_sql state
        (Gs.create_table tname tab.tab_cols)
        (Gs.drop_table tname)
  | Drop_table tname ->
      let { tab_name = tname ; tab_cols = cols ; tab_indexes = indexes } =
        get_table tname in
      Hashtbl.remove tables tname;
      state_add_sql state
        (Gs.drop_table tname)
        (Gs.create_table tname cols ~indexes)
  | Add_column (tname, column_def) ->
      let tab = get_table tname in
      let col = check_added_col_def tname column_def in
      let cols = tab.tab_cols @ [col] in
      check_cols_uniq cols;
      tab.tab_cols <- cols;
      state_add_sql state
        (Gs.add_column tname col)
        (Gs.drop_column tname col.cdc_name)
  | Drop_column
    ({ cr_table = tname; cr_column = cname; cr_kind = ckind } as cr) ->
      check_column_ref_exists cr;
      check_column_ref_can_modify cr "drop";
      let tab = get_table tname in
      let (dropped_cols, cols) = List.partition
        (fun cdc -> cdc.cdc_name = cname && ck_ckc_matches ckind cdc.cdc_kind)
        tab.tab_cols in
      let dropped_col = List.get_single dropped_cols in
      tab.tab_cols <- cols;
      state_add_sql state
        (Gs.drop_column tname cname)
        (Gs.add_column tname dropped_col)
  | Create_index (tname, index_expr) ->
      let tab = get_table tname in
      let indexes = tab.tab_indexes in
      if List.exists (index_expr_eq index_expr) indexes
      then error "table %S: index on %S already exists" tname index_expr
      else
      let iname = make_index_name ~tname ~index_expr in
      tab.tab_indexes <- (iname, index_expr) :: indexes;
      state_add_sql state
        (Gs.create_index ~tname ~index_expr ~iname)
        (Gs.drop_index ~iname)
  | Drop_index (tname, index_expr) ->
      let tab = get_table tname in
      let indexes = tab.tab_indexes in
      if List.exists (index_expr_eq index_expr) indexes
      then
        let (dropped_indexes, indexes) = List.partition
          (index_expr_eq index_expr) indexes in
        let (iname, _ie) = List.get_single dropped_indexes in
        tab.tab_indexes <- indexes;
        state_add_sql state
          (Gs.drop_index ~iname)
          (Gs.create_index ~tname ~index_expr ~iname)
      else error "table %S: index on %S doesn't exist" tname index_expr
  | Rename_table (toldname, tnewname) ->
      let tab = get_table toldname in
      if toldname = tnewname
      then error
        "can't rename table %S to %S, names are equal" toldname tnewname
      else
      Hashtbl.remove tables toldname;
      let tab = { tab with tab_name = tnewname } in
      Hashtbl.add tables tnewname tab;
      state_add_sql state
        (Gs.rename_table ~toldname ~tnewname)
        (Gs.rename_table ~toldname:tnewname ~tnewname:toldname)
  | Rename_column
      ( ({ cr_table = tname ; cr_column = oldcname ; cr_kind = ckind }
          as old_column_ref)
      , newcname
      ) ->
      check_column_ref_exists old_column_ref;
      check_column_ref_can_modify old_column_ref "rename";
      if oldcname = newcname
      then error "table %S: can't rename column %S to %S, names are equal"
        tname oldcname newcname
      else
      let tab = get_table tname in
      let cols = List.map
        (fun cdc ->
           if cdc.cdc_name = oldcname && ck_ckc_matches ckind cdc.cdc_kind
           then { cdc with cdc_name = newcname }
           else cdc
        )
        tab.tab_cols
      in
      check_cols_uniq cols;
      tab.tab_cols <- cols;
      state_add_sql state
        (Gs.rename_column ~tname ~oldcname ~newcname)
        (Gs.rename_column ~tname ~oldcname:newcname ~newcname:oldcname)
  | Modify_column
      ( ({ cr_table = tname ; cr_column = cname ; cr_kind = ckind }
           as column_ref
        )
      , col_mod
      ) ->
      let tab = get_table tname in
      check_column_ref_exists column_ref;
      check_column_ref_can_modify column_ref "modify";
      let new_state = ref None in
      tab.tab_cols <-
        List.map
          (fun cdc ->
             if cdc.cdc_name <> cname
                || not (ck_ckc_matches ckind cdc.cdc_kind)
             then cdc
             else
               check_cdc &
               match col_mod with
               | Cm_set_nullable new_nullable ->
                   if cdc.cdc_nullable = new_nullable
                   then error "table %S: column %S already has nullable=%b"
                     tname cname cdc.cdc_nullable
                   else
                   new_state := Some (state_add_sql state
                     ( Gs.modify_column_nullable ~tname ~cname
                         ~nullable:new_nullable)
                     ( Gs.modify_column_nullable ~tname ~cname
                         ~nullable:(not new_nullable))
                     );
                   { cdc with cdc_nullable = new_nullable }
               | Cm_set_type new_type ->
                   if cdc.cdc_type = new_type
                   then error "table %S: column %S already has type=%S"
                     tname cname cdc.cdc_type
                   else
                     match ckind with
                     | Ck_pk -> assert false
                     | Ck_attr ->
                         let ty = get_type new_type in
                         let new_ddl = get_type_ddl ty in
                         new_state := Some (state_add_sql state
                           ( Gs.modify_column_type ~tname ~cname
                               ~new_ddl)
                           ( Gs.modify_column_type ~tname ~cname
                               ~new_ddl:cdc.cdc_ddl)
                           );
                         { cdc with cdc_type = new_type ; cdc_ddl = new_ddl }
                     | Ck_fk _ -> error
                         "table %S: can't modify type of reference %S"
                         tname cname
          )
          tab.tab_cols;
      begin match !new_state with
      | None -> assert false
      | Some s -> s
      end
  | Create_type (t, ml) ->
      check_type_not_exists t;
      Hashtbl.add types t (create_type_desc t ml);
      state
  | Inherit_type (child_name, base_name) ->
      check_type_not_exists child_name;
      let base = get_type base_name in
      Hashtbl.add types child_name
        { (create_type_desc child_name base.ty_ml_name) with
          ty_parent_type = Some base
        };
      state
  | Pg_of_string (tname, b) ->
      let t = get_type tname in
      begin match t.ty_pg_of_string with
      | None -> t.ty_pg_of_string <- Some b
      | Some _ -> error "type %S: pg_of_string is already defined" tname
      end;
      state
  | Pg_to_string (tname, b) ->
      let t = get_type tname in
      begin match t.ty_pg_to_string with
      | None -> t.ty_pg_to_string <- Some b
      | Some _ -> error "type %S: pg_to_string is already defined" tname
      end;
      state
  | Pg_ddl (tname, b) ->
      let t = get_type tname in
      begin match t.ty_pg_ddl with
      | None -> t.ty_pg_ddl <- Some b
      | Some _ -> error "type %S: pg_ddl is already defined" tname
      end;
      state

let apply_mig_list schema id mig_list =
  let in_state = { em_rev = [] ; prev_up = None } in
  List.rev
  (finish_state &
     List.fold_left (apply_migration schema id) in_state mig_list
  ).em_rev
