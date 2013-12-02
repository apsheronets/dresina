(* post *)

let context =
  { creating_table = None
  ; migration = Queue.create ()
  }

(*
let dump_context ctx =
  Queue.iter
    (function
     | Create_table td ->
         Printf.eprintf "create table %s (%s)" td.td_name
           (String.concat ", "
              (List.map
                 (fun cd ->
                    Printf.sprintf "%s %s" cd.cd_name cd.cd_type
                 )
                 td.td_columns
              )
           )
    )
    ctx.migration
*)

let do_generate migration =
  Struc.expr ~ty:"unit" "()" begin
    Expr.call "register_migration"
      [ Lit.string
          (Filename.chop_suffix (Filename.basename __mlt_filename) ".mlt")
      ; Expr.list (List.map ml_of_migration_item migration)
      ]
  end

let generate lst =
  List.iter
    (function
     | Ml txt -> out txt
     | Dir f -> f context
    )
    lst;
  finish_table_if_any context;
  out (do_generate (list_of_queue context.migration))
