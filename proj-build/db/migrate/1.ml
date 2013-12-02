let () : unit =
  (register_migration ("1") ([ Create_table { td_name = "test" ; td_columns = [ { cd_name = "int_attr" ; cd_type = "integer" };
    { cd_name = "str_attr" ; cd_type = "string" }
  ]
   } ]))
;;
