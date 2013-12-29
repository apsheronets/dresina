(* pre *)

(* TODO: проверять имена таблиц/столбцов на отсутствие кавычек тут. *)

let list_of_queue q =
  List.rev (Queue.fold (fun acc elt -> elt :: acc) [] q)

type context =
  { mutable creating_table :
     (string * column_def Queue.t * (string * int)) option
  ; migration : migration_loc Queue.t
  ; ocaml_functions : (int * string) Queue.t
  ; mutable ocaml_funcno : int
  }

let register_ocaml_migration body ctx =
  let n = ctx.ocaml_funcno in
  ctx.ocaml_funcno <- n + 1;
  Queue.push (n, body) ctx.ocaml_functions;
  n

let finish_table_if_any ctx =
  match ctx.creating_table with
  | None -> ()
  | Some (tname, cols_q, loc) ->
      Queue.push
        { ml_item =
           Mi_special (Create_table
             { td_name = tname
             ; td_columns =
                 let id_col =
                   { cd_name = "id"; cd_type = "id"; cd_nullable = false
                   ; cd_kind = Ck_pk
                   }
                 in
                 id_col :: list_of_queue cols_q
             })
        ; ml_loc = loc
        }
        ctx.migration;
      ctx.creating_table <- None

let create_table1 table_name ctx =
  finish_table_if_any ctx;
  ctx.creating_table <- Some (table_name, Queue.create (), directive_loc ())

let bool_of_nullable = function
  | "null" -> true
  | "notnull" -> false
  | x -> failwith "Expected \"null\" or \"notnull\", found %S" x

let column_def ~cname ~ctype ~cnullable ~ckind =
  { cd_name = cname
  ; cd_type = ctype
  ; cd_nullable = bool_of_nullable cnullable
  ; cd_kind = ckind
  }

let in_table f ctx =
  match ctx.creating_table with
  | None -> failwith "no table creation is active"
  | Some (tname, cols_q, loc) -> f tname cols_q loc

let column3 cname ctype cnullable = in_table & fun _tname cols_q _loc ->
  Queue.push (column_def ~cname ~ctype ~cnullable ~ckind:Ck_attr) cols_q

let reference3 cname tname cnullable = in_table & fun _tname cols_q _loc ->
  Queue.push
    (column_def ~cname ~ctype:"id" ~cnullable
       ~ckind:(Ck_fk tname))
    cols_q

let reference2 tname cnullable ctx =
  reference3 (tname ^ "_id") tname cnullable ctx

(***********)

let common v = fun ctx ->
  finish_table_if_any ctx;
  Queue.push { ml_item = v ; ml_loc = directive_loc () } ctx.migration

let common_spec v = common (Mi_special v)

let add_column4 tname cname ctype cnullable = common_spec &
  Add_column (tname, (column_def ~cname ~ctype ~cnullable ~ckind:Ck_attr))

let add_reference4 tname refcolumn reftable cnullable = common_spec &
  Add_column
    ( tname
    , (column_def ~cname:refcolumn ~ctype:"id" ~cnullable
         ~ckind:(Ck_fk reftable))
    )

let add_reference3 tname reftable cnullable =
  add_reference4 tname (reftable ^ "_id") reftable cnullable

let column_ref ~tname ~cname ~ckind =
  { cr_table = tname
  ; cr_column = cname
  ; cr_kind = ckind
  }

let drop_column2 tname cname = common_spec &
  Drop_column (column_ref ~tname ~cname ~ckind:Ck_attr)

let drop_reference3 tname refcolumn reftable = common_spec &
  Drop_column (column_ref ~tname ~cname:refcolumn ~ckind:(Ck_fk reftable))

let drop_reference2 tname reftable ctx =
  drop_reference3 tname (reftable ^ "_id") reftable ctx

let create_index2 tname index_expr = common_spec &
  Create_index (tname, index_expr)

let drop_index2 tname index_expr = common_spec &
  Drop_index (tname, index_expr)

let rename_table2 toldname tnewname = common_spec &
  Rename_table (toldname, tnewname)

let modify_column3 tname cname modif = common_spec &
  let cm =
    match modif with
    | "null" -> Cm_set_nullable true
    | "notnull" -> Cm_set_nullable false
    | new_type -> Cm_set_type new_type
  in
    Modify_column ((column_ref ~tname ~cname ~ckind:Ck_attr), cm)

let modify_reference4 tname refcolumn reftable modif = common_spec &
  let cm =
    match modif with
    | "null" -> Cm_set_nullable true
    | "notnull" -> Cm_set_nullable false
    | _ -> failwith
        "references can be modified only to \"null\" or \"notnull\""
  in
    Modify_column
      ( (column_ref ~tname ~cname:refcolumn ~ckind:(Ck_fk reftable))
      , cm
      )

let modify_reference3 tname reftable modif ctx =
  modify_reference4 tname (reftable ^ "_id") reftable modif ctx

let rename_column3 tname cname cnewname = common_spec &
  Rename_column ((column_ref ~tname ~cname ~ckind:Ck_attr), cnewname)

let rename_reference4 tname refcolumn reftable cnewname = common_spec &
  Rename_column
    ((column_ref ~tname ~cname:refcolumn ~ckind:(Ck_fk reftable)), cnewname)

let rename_reference3 tname reftable newcolumn ctx =
  rename_reference4 tname (reftable ^ "_id") reftable newcolumn ctx

let mig_sql dir body ctx = (common &
  Mi_generic (dir, Ema_sql (strip_line_directive body))
  ) ctx

let up_sql0b = mig_sql Md_up

let down_sql0b = mig_sql Md_down

let up_ocaml0b body ctx = common (
  let funcno = register_ocaml_migration body ctx in
  Mi_generic (Md_up, Ema_ocaml funcno)) ctx

let down_ocaml0b body ctx = common (
  let funcno = register_ocaml_migration body ctx in
  Mi_generic (Md_down, Ema_ocaml funcno)) ctx

let drop_table1 tname = common_spec &
  Drop_table tname

(************************************************)

let create_type2 ty_name ml_type = common_spec &
  Create_type (ty_name, ml_type)

let pg_of_string1b ty b = common_spec &
  Pg_of_string (ty, b)

let pg_to_string1b ty b = common_spec &
  Pg_to_string (ty, b)

let pg_ddl2 ty ddl = common_spec &
  Pg_ddl (ty, ddl)

let inherit_type2 ty_child ty_base = common_spec &
  Inherit_type (ty_child, ty_base)
