open Cd_All
open Strings.Latin1
open Schema_types
module Cg = Codegen
let sprintf = Printf.sprintf

let gather f = fun a ->
  match stage with
  | `Gather -> f a
  | `Code -> ()

let code f = fun a ->
  match stage with
  | `Code -> f a
  | `Gather -> ()

type context =
  { mutable cols : column_def_checked list option
  ; fetchers_names : (string, unit) Hashtbl.t
  }

let schema = lazy
  (Schema_tm.from_file (Filename.concat "proj-build" schema_bin_fname))

let table1 tname = gather & fun ctx ->
  match ctx.cols with
  | Some _ -> failwith "duplicate 'table' directive"
  | None ->
      let schema = Lazy.force schema in
      let tab =
        try Hashtbl.find schema.s_tables tname
        with Not_found -> failwith
          "Can't find table %S in current schema" tname
      in
      ctx.cols <- Some tab.tab_cols

(********************************************************************)

let () = code out "open Proj_common\n"

let model_desc = Memo.create ~eq:String.eq begin fun model_name ->
  let fname = "proj-build/app/models/" ^ model_name ^ ".desc.bin" in
  Opt_list_cdc_tm.from_file fname
end

let self_model_name =
  Filename.chop_suffix (Filename.basename __mlt_filename) ".mlt"

(********************************************************************)



(* assuming "data : Postgresql.result; row : int" in environment *)
let object_of_cols cols_inds =
  "object\n" ^
  (String.concat "" &
     List.map
       (fun (col, ind) ->
          let cname = col.cdc_name in
          let ctype = col.cdc_type in
          let ctm_ml =
            "(Migrate_types.(" ^
            Migrate_types.ml_of_column_type_modifier col.cdc_type_mod ^ "))"
          in
          let v =
            sprintf "Schema_code.pg_%s_of_string %s (data#getvalue row %i)"
              ctype ctm_ml ind
          in
          let v_opt =
            if col.cdc_nullable
            then sprintf
              "if data#getisnull row %i then None else Some (%s)"
              ind v
            else v
          in
          sprintf
            "val mutable %s =\n%s\n\
             method %s = %s\n\
             method set_%s x = %s <- x\n"
            cname v_opt
            cname cname
            cname cname
       )
       cols_inds
  ) ^ "\nend\n"


(* creates "object 'mnames' : collection .. end",
   assuming "data : Postgresql.result" in environment.
   todo: %plural for manual 'mname'->'mnames'.
 *)
let create_instances_simple mname cols_inds =
  let mnames = Cg.Expr.lid (mname ^ "s") in
  "let coll = new Collection.map2_id_ord in\n\
   let add = coll#add in\n\
   for row = 0 to data#ntuples - 1 do\n" ^
  Cg.indent 2 begin
    "add ~ord:row begin\n" ^
    Cg.indent 2 begin
      object_of_cols cols_inds
    end
    ^ "\nend"
  end
  ^
  "\ndone;\n" ^
  sprintf "object method %s = coll end" mnames


let to_io code =
  "begin try return begin\n" ^ Cg.indent 2 code ^
  "\nend with e -> fail e end\n"

(* simple == for one table, one model, no special columns selection *)
let generate_fetcher_simple ~qname ~body ~ctx =
  if Hashtbl.mem ctx.fetchers_names qname
  then failwith "Query with the same name is already defined"
  else
  let cols =
    match model_desc self_model_name with
    | None -> failwith "Model have no description (%%table missed?)"
    | Some cols -> cols
  in
  let cols_sql = String.concat ", " & List.map (fun c -> c.cdc_name) cols in
  let cols_inds = List.mapi (fun i c -> (c, i)) cols in
  let sql = "select " ^ cols_sql ^ " " ^ body
  in
  Hashtbl.add ctx.fetchers_names qname ();
  out &
  Cg.Struc.func qname ["()"] begin
    "Database.pg_query_result\n" ^
    Cg.Lit.string sql ^ "\n" ^
    ">>= fun data ->\n" ^
    (to_io &
     create_instances_simple self_model_name cols_inds
    )
  end

(********************************************************************)

let data1b qname body = code & fun ctx ->
  let body = Cg.strip_line_directive body in
  generate_fetcher_simple ~qname ~body ~ctx
