(* post *)

let context =
  { creating_table = None
  ; migration = Queue.create ()
  ; ocaml_functions = Queue.create ()
  ; ocaml_funcno = 0
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

let do_generate migration ocaml_functions =
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
  let mig_func_name func_no = "mig_func_" ^ string_of_int func_no in
  Struc.items
    ( [ "open Migrate_types" ]
    @ List.map
        (fun (func_no, body) ->
           sprintf "let %s (conn : Dbi_pg.connection) : unit =\n%s\n"
             (Expr.lid (mig_func_name func_no)) body
        )
        ocaml_functions
    @ [ Struc.func "register" ["()"] begin
          Expr.seq
          ( Expr.call_gen ~newlines:true "Migrations.register_migration"
              [ Lit.string mig_id
              ; Expr.list (List.map ml_of_migration_loc migration)
              ]
            ::
            List.map
              (fun (func_no, _body) ->
                 Expr.call_gen ~newlines:true
                   "Migrations.register_ocaml_function"
                   [ Lit.string mig_id
                   ; Lit.int func_no
                   ; mig_func_name func_no
                   ]
              )
              ocaml_functions
          )
        end
      ]
    )

let generate lst =
  List.iter
    (function
     | Ml txt -> out txt
     | Dir f -> f context
    )
    lst;
  finish_table_if_any context;
  out
    (do_generate
       (list_of_queue context.migration)
       (list_of_queue context.ocaml_functions)
    )
