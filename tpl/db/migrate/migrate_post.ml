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

let do_generate migration ocaml_functions : struc_item list =
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
  [ Struc.open_ ["Migrate_types"] ]
  @ List.map
      (fun (func_no, (fname, lineno, bodycode)) ->
         Struc.let_
           (Patt.func
              (mig_func_name func_no)
              [ Patt.ascribe
                  (Patt.lid "conn")
                  (Typ.prim ~mod_path:["Dbi_pg"] "connection")
              ]
           )
           ~typ:(Typ.prim "unit")
           (Expr.linedir fname lineno & Expr.inj bodycode)
      )
      ocaml_functions
  @ [ Struc.func "register" [Patt.unit] &
        Expr.seq
        ( Expr.call_mod ["Migrations"] "register_migration"
            [ Expr.string mig_id
            ; Expr.list (List.map expr_of_migration_loc migration)
            ]
          ::
          List.map
            (fun (func_no, _body) ->
               Expr.call_mod ["Migrations"] "register_ocaml_function"
                 [ Expr.string mig_id
                 ; Expr.int func_no
                 ; Expr.lid & mig_func_name func_no
                 ]
            )
            ocaml_functions
        )
    ]


let generate lst =
  List.iter
    (function
     | Ml txt -> out_raw txt
     | Dir f -> f context
    )
    lst;
  finish_table_if_any context;
  out
    (do_generate
       (list_of_queue context.migration)
       (list_of_queue context.ocaml_functions)
    )
