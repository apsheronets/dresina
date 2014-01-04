open Cd_All
open Strings.Latin1
open Migrate_types
open Schema_types
open Printf


let quote () s =
  assert (not (String.contains s '"'));
  "\"" ^ s ^ "\""

let col_type_ddl ~cdc_type ~cdc_type_mod =
  let get_ddl =
    try Hashtbl.find Schema_code.ddl_of_type cdc_type
    with Not_found ->
      failwith ("Generate_sql: no entry in 'ddl_of_type' for " ^ cdc_type)
  in
  get_ddl cdc_type_mod

let col_ddl c =
  sprintf "%a %s %s %s"
    quote c.cdc_name
    (col_type_ddl ~cdc_type:c.cdc_type ~cdc_type_mod:c.cdc_type_mod)
    (if c.cdc_nullable then "null" else "not null")
    (if c.cdc_kind = Ckc_pk then "primary key" else "")

let create_fk ~tname ~fk_db_name ~cname ~reftable =
  sprintf "alter table %a add constraint %a \
             foreign key (%a) references %a (id)"
    quote tname
    quote fk_db_name
    quote cname
    quote reftable

let methods =
  object (self)

    method create_index ~tname ~index_expr ~iname =
      sprintf "create index %a on %a (%s)"
        quote iname
        quote tname
        index_expr

    method drop_index ~iname =
      sprintf "drop index %a"
        quote iname

    method create_table ~indexes tname cols =
      String.concat "; "
       (
        [ sprintf "create table %a (%s)"
            quote tname
            (String.concat " , " & List.map col_ddl cols)
        ]
        @
          List.map
            (fun (iname, index_expr) ->
               self#create_index ~tname ~index_expr ~iname
            )
            indexes
        @
          List.map_filter
            (fun c ->
               match c.cdc_kind with
               | Ckc_pk | Ckc_attr -> None
               | Ckc_fk (fk_db_name, reftable) -> Some begin
                   create_fk ~tname ~fk_db_name ~cname:c.cdc_name ~reftable
                 end
            )
            cols
       )

    method drop_table tname =
      sprintf "drop table %a"
        quote tname

    method add_column tname col =
      String.concat "; "
      ( [ sprintf "alter table %a add column %s"
            quote tname
            (col_ddl col)
        ]
      @ match col.cdc_kind with
        | Ckc_pk -> assert false
        | Ckc_attr -> []
        | Ckc_fk (fk_db_name, reftable) ->
            [ create_fk ~tname ~fk_db_name ~cname:col.cdc_name ~reftable ]
      )

    method drop_column tname cname =
      sprintf "alter table %a drop column %a"
        quote tname
        quote cname

    method rename_table ~toldname ~tnewname =
      sprintf "alter table %a rename to %a"
        quote toldname
        quote tnewname

    method rename_column ~tname ~oldcname ~newcname =
      sprintf "alter table %a rename column %a to %a"
        quote tname
        quote oldcname
        quote newcname

    method modify_column_nullable ~tname ~cname ~nullable =
      sprintf "alter table %a alter column %a %s not null"
        quote tname
        quote cname
        (if nullable then "drop" else "set")

    method modify_column_type ~tname ~cname ~new_type ~new_type_mod =
      sprintf "alter table %a alter column %a type %s"
        quote tname
        quote cname
        (col_type_ddl ~cdc_type:new_type ~cdc_type_mod:new_type_mod)
  end
