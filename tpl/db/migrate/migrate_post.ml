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
  let fname = Filename.chop_suffix (Filename.basename __mlt_filename) ".mlt" in
  let (mig_id, _, _) = String.split_by_first
    (function '0'..'9' -> false | _ -> true)
    fname
  in
  if mig_id = ""
  then
    failwith "migration source filename must begin with digits that form its \
              migration identifier, while file has name %S" fname
  else
  "open Migrate_types\n" ^
  Struc.func "register" ["()"] begin
    Expr.call_gen ~newlines:true "Migrations.register_migration"
      [ Lit.string mig_id
      ; Expr.list (List.map ml_of_migration_loc migration)
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
