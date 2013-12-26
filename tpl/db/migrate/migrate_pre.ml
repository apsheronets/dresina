(* pre *)

(* TODO: проверять имена таблиц/столбцов на отсутствие кавычек тут. *)

let list_of_queue q =
  List.rev (Queue.fold (fun acc elt -> elt :: acc) [] q)

type context =
  { mutable creating_table :
     (string * column_def Queue.t * (string * int)) option
  ; migration : migration_loc Queue.t
  }

let finish_table_if_any ctx =
  match ctx.creating_table with
  | None -> ()
  | Some (tname, cols_q, loc) ->
      Queue.push
        { ml_item =
           Create_table
             { td_name = tname
             ; td_columns = list_of_queue cols_q
             }
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

let add_column4 tname cname ctype cnullable = common &
  Add_column (tname, (column_def ~cname ~ctype ~cnullable ~ckind:Ck_attr))

let add_reference4 tname refcolumn reftable cnullable = common &
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

let drop_column2 tname cname = common &
  Drop_column (column_ref ~tname ~cname ~ckind:Ck_attr)

let drop_reference3 tname refcolumn reftable = common &
  Drop_column (column_ref ~tname ~cname:refcolumn ~ckind:(Ck_fk reftable))

let drop_reference2 tname reftable ctx =
  drop_reference3 tname (reftable ^ "_id") reftable ctx

let create_index2 tname index_expr = common &
  Create_index (tname, index_expr)

let drop_index2 tname index_expr = common &
  Drop_index (tname, index_expr)

let rename_table2 toldname tnewname = common &
  Rename_table (toldname, tnewname)

let modify_column3 tname cname modif = common &
  let cm =
    match modif with
    | "null" -> Cm_set_nullable true
    | "notnull" -> Cm_set_nullable false
    | new_type -> Cm_set_type new_type
  in
    Modify_column ((column_ref ~tname ~cname ~ckind:Ck_attr), cm)

let modify_reference4 tname refcolumn reftable modif = common &
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

let rename_column3 tname cname cnewname = common &
  Rename_column ((column_ref ~tname ~cname ~ckind:Ck_attr), cnewname)

let rename_reference4 tname refcolumn reftable cnewname = common &
  Rename_column
    ((column_ref ~tname ~cname:refcolumn ~ckind:(Ck_fk reftable)), cnewname)

let rename_reference3 tname reftable newcolumn ctx =
  rename_reference4 tname (reftable ^ "_id") reftable newcolumn ctx

let up_sql0b body = common &
  Generic (Md_up, Ml_sql, body)

let down_sql0b body = common &
  Generic (Md_down, Ml_sql, body)

let up_ocaml0b body = common &
  Generic (Md_up, Ml_ocaml, body)

let down_ocaml0b body = common &
  Generic (Md_down, Ml_ocaml, body)

let drop_table1 tname = common &
  Drop_table tname

(************************************************)

let create_type2 ty_name ml_type = common &
  Create_type (ty_name, ml_type)

let pg_of_string1b ty b = common &
  Pg_of_string (ty, b)

let pg_to_string1b ty b = common &
  Pg_to_string (ty, b)

let pg_ddl2 ty ddl = common &
  Pg_ddl (ty, ddl)

let inherit_type2 ty_child ty_base = common &
  Inherit_type (ty_child, ty_base)
