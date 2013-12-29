open Migrate_types

(* update it with data types modifications: *)

let marshal_tag = "schema v.1 " ^ Migrate_types.all_types_tag

type type_desc =
  { ty_name : string
  ; ty_ml_name : string
  ; mutable ty_pg_of_string : string option
  ; mutable ty_pg_to_string : string option
  ; mutable ty_pg_ddl : string option
  ; mutable ty_parent_type : type_desc option
  }

type column_kind_checked =
| Ckc_pk
| Ckc_attr
| Ckc_fk of (string * string)
    (* Ckc_fk (fk_db_name, reftable) *)

type column_def_checked =
  { cdc_name : string
  ; cdc_type : string
  ; cdc_nullable : bool
  ; cdc_kind : column_kind_checked
  ; cdc_ddl : string
  }

type index_db_ident = string

type table_desc =
  { tab_name : string
  ; mutable tab_cols : column_def_checked list
  ; mutable tab_indexes : (index_db_ident * index_expr) list
  }

type schema =
  { s_types : (string, type_desc) Hashtbl.t
  ; s_tables : (string, table_desc) Hashtbl.t
  }

let create_empty_schema () =
  { s_types = Hashtbl.create 67
  ; s_tables = Hashtbl.create 67
  }
