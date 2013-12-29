open Cd_All
open Strings.Latin1
open Migrate_types
open Schema_types
open Printf

let db_ident_max_len = 32

let form_db_ident_of_string s =
  String.concat "_" &
  String.split
    (function 'a'..'z' | 'A'..'Z' | '0'..'9' -> false | _ -> true)
    s

let make_unique_name base suffix is_unique =
  let base = form_db_ident_of_string base in
  let rec get_ident base opt_i =
    let r = base
      ^ (match opt_i with None -> "" | Some i -> string_of_int i)
      ^ suffix in
    if String.length r > db_ident_max_len
    then get_ident (String.sub base 0 (String.length base - 1)) opt_i
    else r
  in
    let no_counter = get_ident base None in
    if is_unique no_counter
    then no_counter
    else
      let rec loop i =
        let ident = get_ident base (Some i) in
        if is_unique ident
        then ident
        else loop (i + 1)
      in
        loop 2

let quote () s =
  assert (not (String.contains s '"'));
  "\"" ^ s ^ "\""

let create_index ~tname ~index_expr ~iname =
  sprintf "create index %a on %a (%s)"
    quote iname
    quote tname
    index_expr

let drop_index ~iname =
  sprintf "drop index %a"
    quote iname

let col_ddl c =
  sprintf "%a %s %s %s"
    quote c.cdc_name
    c.cdc_ddl
    (if c.cdc_nullable then "null" else "not null")
    (if c.cdc_kind = Ckc_pk then "primary key" else "")

let create_fk ~tname ~fk_db_name ~cname ~reftable =
  sprintf "alter table %a add constraint %a \
             foreign key (%a) references %a (id)"
    quote tname
    quote fk_db_name
    quote cname
    quote reftable

let create_table ?(indexes = []) tname cols =
  String.concat "; "
   (
    [ sprintf "create table %a (%s)"
        quote tname
        (String.concat " , " & List.map col_ddl cols)
    ]
    @
      List.map
        (fun (iname, index_expr) ->
           create_index ~tname ~index_expr ~iname
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

let drop_table tname =
  sprintf "drop table %a"
    quote tname

let add_column tname col =
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

let drop_column tname cname =
  sprintf "alter table %a drop column %a"
    quote tname
    quote cname

let rename_table ~toldname ~tnewname =
  sprintf "alter table %a rename to %a"
    quote toldname
    quote tnewname

let rename_column ~tname ~oldcname ~newcname =
  sprintf "alter table %a rename column %a to %a"
    quote tname
    quote oldcname
    quote newcname

let modify_column_nullable ~tname ~cname ~nullable =
  sprintf "alter table %a alter column %a %s not null"
    quote tname
    quote cname
    (if nullable then "drop" else "set")

let modify_column_type ~tname ~cname ~new_ddl =
  sprintf "alter table %a alter column %a type %s"
    quote tname
    quote cname
    new_ddl
