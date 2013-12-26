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

type column_def_checked =
  { cdc_name : string
  ; cdc_type : string
  ; cdc_nullable : bool
  ; cdc_kind : column_kind
  }

type table_desc =
  { tab_name : string
  ; mutable tab_cols : column_def_checked list
  ; mutable tab_indexes : index_expr list
  }

type schema =
  { s_types : (string, type_desc) Hashtbl.t
  ; s_tables : (string, table_desc) Hashtbl.t
  }
