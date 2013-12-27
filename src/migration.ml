open Cd_All
open Staging
open Ml_comp
open Common
open Strings.Latin1

let () = stage
  ~pre:[ "proj-build/internal/common/tagged_marshal.ml.module"
       ; "tpl/internal/common/ocaml_type_gen_pre.ml"
       ]
  ~post:["tpl/internal/common/ocaml_type_gen_post.ml"]
  ~mlt:"tpl/db/migrate/migrate_types.mlt"
  "tpl/db/migrate/migrate_types.ml"

let (migrations, last_migration_id) =
  let src_path = "proj/db/migrate"
  and dst_path = "proj-build/db/migrate" in
  let ids = Hashtbl.create 127
  and last_migration_id = ref "" in
  src_path |> readdir_list |> List.sort String.compare |>
  (fun lst -> "0_initial.mlt" :: lst) |>
  List.uniq ~eq:String.eq |>
  List.map_filter
    (fun n ->
       if Filename.check_suffix n ".mlt"
       then
         let (id, _, _) = String.split_by_first
           (function '0'..'9' -> false | _ -> true)
           n
         in
         if id = ""
         then failwith "migration %S must begin from some digits that form \
                        its migration identifier" n
         else
         let () =
           if Hashtbl.mem ids id
           then failwith "duplicate migration identifier %S" id
           else Hashtbl.add ids id ()
         in
         let () =
           if id > !last_migration_id
           then last_migration_id := id
           else ()
         in
         let src = src_path // n
         and dst = dst_path //
           ("migration_" ^ change_suffix ~place:"migration" n ".mlt" ".ml")
         in Some (src, dst)
       else
         None
    )
  |> fun migrations ->
  (migrations, !last_migration_id)

let () = List.iter
  (fun (src, dst) ->
     stage
       ~pre:[ "proj-build/internal/common/tagged_marshal.ml.module"
            ; "proj-build/internal/common/codegen.ml.module"
            ; "tpl/db/migrate/migrate_types.ml"
            ; "tpl/db/migrate/migrate_pre.ml"
            ]
       ~post:["tpl/db/migrate/migrate_post.ml"]
       ~mlt:src
       dst
  )
  migrations


let () =
  let dep_migs = "MIGRATIONS" in
  Hashtbl.add Make.virt_deps dep_migs begin
    digest_string_list &
    List.map
      (fun (src, _dst) -> src)
      migrations
  end;
  let dst = "proj-build/db/migrate/register_all_migrations.ml" in
  Make.make [dst] ~virtdeps:[dep_migs] [] begin fun () ->
    let contents =
      Cg.Struc.items &
        [ Cg.Struc.func "register" ["()"] &
          Cg.Expr.seq &
          List.map
            (fun (_src, dst) ->
               let modname = String.capitalize & Filename.basename &
                 change_suffix ~place:"register_all_migrations" dst ".ml" "" in
               Cg.Expr.call_gen (Cg.Expr.modqual modname "register")
                 [ "()"
                 ]
            )
            migrations
        ; Cg.Struc.expr "last_migration_id" & Cg.Lit.string last_migration_id
        ]
    in
      Filew.spit_bin dst contents
  end



let register_make_rules () = ()
