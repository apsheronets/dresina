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
         if String.length id > 100
         then failwith "migration %S has too long migration id, \
                        please limit it to 100 digits max" n
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
            ; "tpl/db/migrate/migrate_types.ml"
            ; "tpl/db/migrate/migrate_pre.ml"
            ]
       ~post:["tpl/db/migrate/migrate_post.ml"]
       ~mlt:src
       dst
  )
  migrations


let reg_all = "proj-build/db/migrate/register_all_migrations.ml"

let () =
  let dep_migs = "MIGRATIONS" in
  Hashtbl.add Make.virt_deps dep_migs begin
    digest_string_list &
    List.map
      (fun (src, _dst) -> src)
      migrations
  end;
  let dst = reg_all in
  Make.make [dst] ~virtdeps:[dep_migs] [] begin fun () ->
    let contents =
        [ Cg.Struc.func "register" [Cg.Patt.unit] &
          Cg.Expr.seq &
          List.map
            (fun (_src, dst) ->
               let modname = String.capitalize & Filename.basename &
                 change_suffix ~place:"register_all_migrations" dst ".ml" "" in
               Cg.Expr.call_mod [modname] "register"
                 [ Cg.Expr.unit
                 ]
            )
            migrations
        ; Cg.Struc.expr "last_migration_id" & Cg.Expr.string last_migration_id
        ]
    in
      Filew.spit_bin dst & Cg.Implem.to_string contents
  end


let migrate_targets = reg_all :: List.map (fun (_src, dst) -> dst) migrations

let schema_targets =
  ["proj-build/db/schema_code.ml"; "proj-build/db/schema.bin"]

let () =
  let ml =
    [ "proj-build/internal/common/migrations.ml"
    ; "proj-build/internal/common/schema_types.ml"
    ; "proj-build/internal/common/apply_migrations.ml"
    ; "proj-build/internal/common/proj_common.ml"
    ] in
  Make.make schema_targets (ml @ migrate_targets) begin fun () ->
    sys_command_ok "make -C proj-build .depend db/schema_code.ml -j1"
  end


let register_make_rules () = ()
