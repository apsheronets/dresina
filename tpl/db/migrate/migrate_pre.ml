(* pre *)

let list_of_queue q =
  List.rev (Queue.fold (fun acc elt -> elt :: acc) [] q)

type context =
  { mutable creating_table : (string * column_def Queue.t) option
  ; migration : migration_item Queue.t
  }

let finish_table_if_any ctx =
  match ctx.creating_table with
  | None -> ()
  | Some (tname, cols_q) ->
      Queue.push
        (Create_table
           { td_name = tname
           ; td_columns = list_of_queue cols_q
           }
        )
        ctx.migration;
      ctx.creating_table <- None

let create_table1 table_name ctx =
  finish_table_if_any ctx;
  ctx.creating_table <- Some (table_name, Queue.create ())

let column2 cname ctype ctx =
  match ctx.creating_table with
  | None -> failwith "no table creation is active"
  | Some (_tname, cols_q) ->
      Queue.push
        { cd_name = cname
        ; cd_type = ctype
        }
        cols_q
