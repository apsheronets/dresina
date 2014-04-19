open Migrate_types

let schema_bin_fname = "db/schema.bin"
and schema_code_fname = "db/schema_code.ml"

(* update it with data types modifications: *)

let tag = " v.1 (" ^ Migrate_types.all_types_tag ^ ")"

type type_desc =
  { ty_name : string
  ; ty_ml_name : string
  ; mutable ty_pg_of_string : body option
  ; mutable ty_pg_to_string : body option
  ; mutable ty_pg_ddl : body option
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
  ; cdc_type_mod : column_type_modifier
  }

module Opt_string_and_list_cdc_tm = Tagged_marshal.Make(
   struct
     type t = (string * column_def_checked list) option
     let tag = "opt_string_and_list_cdc" ^ tag
   end
  )

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

module Schema_tm = Tagged_marshal.Make
  (struct type t = schema let tag = "schema" ^ tag end)

let create_empty_schema () =
  { s_types = Hashtbl.create 67
  ; s_tables = Hashtbl.create 67
  }
