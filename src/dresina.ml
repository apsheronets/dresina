open Cd_All
open Strings.Latin1
open Staging
open Ml_comp
open Common

let dbpkgs = ["amall.dbi"]
let webpkgs = ["amall"]
let allpkgs = List.uniq ~eq:String.eq (dbpkgs @ webpkgs)

let () =
  let src = "src/codegen.ml"
  and dst = "proj-build/internal/common/codegen.ml" in
  Ml_make.glue [src] dst

let module_name_of_fname fn =
  let place = "module_name_of_fname" in
  let n = Filename.basename fn in
  let n = change_suffix ~place n ".ml" "" in
  let m = String.capitalize n in
  Codegen.check_uid ~place m;
  m

let () = List.iter
  (fun fn ->
     Ml_make.wrap_in_module ~m:(module_name_of_fname fn) fn (fn ^ ".module")
  )
  [ "proj-build/internal/common/codegen.ml"
  ; "proj-build/internal/common/tagged_marshal.ml"
  ; "proj-build/db/migrate/migrate_types.ml"
  ; "proj-build/internal/common/schema_types.ml"
  ]

let () = stage_paths
  ~pkgs:dbpkgs
  ~rel_path:"config"
  ~pre:["database_pre.ml"]
  ~post:["database_post.ml"]
  ~mlt:"database.mlt"
  "database_config.ml"

let () = stage_multi_paths
  ~pkgs:webpkgs
  ~rel_path:"config"
  ~pre:["routes_pre.ml"]
  ~post:["routes_post.ml"]
  ~mlt:"routes.mlt"
  [("routes", "route.ml"); ("routing", "routing.ml")]

let server_bin = "dresina-server" (* keep in sync with tpl/Makefile! *)
let server_path_bin = "proj-build" // server_bin

let con_path = "app/controllers"
let controllers_mls =
  Array.filter_to_list
    (fun fn -> Filename.check_suffix fn ".ml")
    (Sys.readdir ("proj" // con_path))

let () = List.iter
  (fun con_ml ->
     Ml_make.glue
       [ "tpl"  // con_path // "controller_pre.ml"
       ; "proj" // con_path // con_ml
       ; "tpl"  // con_path // "controller_post.ml"
       ] &
     "proj-build" // con_path // con_ml
  )
  controllers_mls

let () = Migration.register_make_rules ()

let () = View.register_make_rules ()

let () = Model.register_make_rules ()

let make_copy_from_tpl fn =
  let src = "tpl" // fn
  and dst = "proj-build" // fn in
  Ml_make.glue [src] dst

let () =
  List.iter make_copy_from_tpl
    ("Makefile" ::
     "db/migrate/migrate_types.ml" ::
       List.map (fun n -> "internal" // n)
       [ "common/proj_common.ml"; "server/main_pre.ml"; "server/main_post.ml"
       ; "server/database.ml"; "server/psql.ml"
       ; "server/command_db_create.ml"; "server/command_db_drop.ml"
       ; "server/viewHelpers.ml"
       ; "common/tagged_marshal.ml"
       ; "common/migrations.ml"; "make_schema/make_schema.ml"
       ; "common/schema_types.ml"
       ; "server/command_db_migrate.ml"
       ; "server/generate_sql.ml"
       ; "common/apply_migrations.ml"
       ; "server/command_db_rollback.ml"
       ; "common/collection.ml"
       ]
    )

let () =
  let src = "tpl/db/migrate/pg_initial_migration.mlt"
  and dst = "proj/db/migrate/0_initial.mlt" in
  Make.make1 dst [src] begin fun () ->
    Filew.copy_file src dst
  end

(***********)

let codegen_action () =
  Make.do_make ()

let make_action () =
  codegen_action ();
  sys_command_ok "make -C proj-build .depend all -j1"

let run_server () =
  make_action ();
  Printf.printf "running server %S...\n%!" server_path_bin;
  sys_command_ok server_path_bin

let default_cmd_action =
  `Help (`Plain, None)

let clean_cmd_action () =
  Make.do_clean ()

(***********)

open Cmdliner

let server_cmd =
  let doc = "Run server on http://localhost:4000/" in
  Term.(pure run_server),
  Term.info "server" ~doc

let default_cmd =
  let doc = "web stuff" in
  Term.(ret (pure default_cmd_action)),
  Term.info "dresina" ~doc

let clean_cmd =
  let doc = "clean generated files" in
  Term.(pure clean_cmd_action),
  Term.info "clean" ~doc

let dump_deps_cmd =
  let doc = "dump make dependencies" in
  Term.(pure Make.dump_deps),
  Term.info "dump-deps" ~doc

let codegen_cmd =
  let doc = "generate code" in
  Term.(pure codegen_action),
  Term.info "codegen" ~doc

let make_cmd =
  let doc = "generate code and build server" in
  Term.(pure make_action),
  Term.info "make" ~doc

let cmds = [server_cmd; clean_cmd; dump_deps_cmd; codegen_cmd; make_cmd]

let () = match Term.eval_choice default_cmd cmds with
| `Error _ -> exit 1
| `Ok action -> ignore (action ()); exit 0
| `Help | `Version -> exit 0
