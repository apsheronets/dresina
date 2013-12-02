type column_def =
  { cd_name : string
  ; cd_type : string
  }
;;
let ml_of_column_def r =
  "{ " ^
  "cd_name = " ^ (ml_of_string r.cd_name) ^ " ; " ^ 
  "cd_type = " ^ (ml_of_string r.cd_type) ^ " }"
# 5 "tpl/db/migrate/migrate_types.mlt"

type column_def_list = column_def list

let ml_of_column_def_list lst =
  Expr.list (List.map ml_of_column_def lst)

# 14 "tpl/db/migrate/migrate_types.mlt"

type table_def =
  { td_name : string
  ; td_columns : column_def_list
  }
;;
let ml_of_table_def r =
  "{ " ^
  "td_name = " ^ (ml_of_string r.td_name) ^ " ; " ^ 
  "td_columns = " ^ (ml_of_column_def_list r.td_columns) ^ " }"
type migration_item =
| Create_table of table_def
;;
let ml_of_migration_item = function
| Create_table x -> "Create_table " ^ (ml_of_table_def (x))
