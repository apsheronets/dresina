open Cd_All
open Strings.Latin1
open Staging
open Ml_comp
open Common

let dbpkgs = ["amall.dbi"]
let webpkgs = ["amall"]
let allpkgs = List.uniq ~eq:String.eq (dbpkgs @ webpkgs)

let () = stage
  ~pre:["tpl/internal/ocaml_type_gen_pre.ml"]
  ~post:["tpl/internal/ocaml_type_gen_post.ml"]
  ~mlt:"tpl/db/migrate/migrate_types.mlt"
  "tpl/db/migrate/migrate_types.ml"

let () = stage_paths
  ~rel_path:"db/migrate"
  ~pre:["migrate_types.ml"; "migrate_pre.ml"]
  ~post:["migrate_post.ml"]
  ~mlt:"1.mlt"
  "1.ml"

(*
let () = copy_mls_to_ml
  ~files:["tpl/config/database_pre.ml"; "proj/config/database.ml"]
  ~out:"proj-build/config/database.ml"
*)

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

let () = Ml_make.glue
  ["tpl/internal/proj_common.ml"]
  "proj-build/internal/proj_common.ml"

(* not used now, since using make + ocamldep-sorter to build server
let _cmx : string = Ml_make.compile_ml_to_obj
  ~pkgs:allpkgs
  ~deps:[]
  "proj-build/internal/proj_common.ml"

let common_objs = [ "proj-build/internal/proj_common.cmx" ]

let controller_deps = common_objs  (* todo *)

let controller_objs = List.map
  (fun ml ->
     Ml_make.compile_ml_to_obj
       ~deps:controller_deps
       ~pkgs:allpkgs
       ~incl:["proj-build/internal"] &
     "proj-build" // con_path // ml
  )
  controllers_mls

let () = Ml_make.compile_mls_to_nat
  ~pkgs:allpkgs
  ~incl:["proj-build/internal"; "proj-build" // con_path]
  ~pre_objs:(common_objs @ controller_objs)
  [ "config/route.ml"; "main.ml" ]
  server_path_bin
*)

let () = View.register_make_rules ()

let make_copy_from_tpl fn =
  let src = "tpl" // fn
  and dst = "proj-build" // fn in
  Make.make1 dst [src] begin fun () ->
    Filew.copy_file src dst
  end

let () =
  List.iter make_copy_from_tpl
    [ "Makefile"; "internal/main_pre.ml"; "internal/main_post.ml"
    ]

let () = Ml_make.glue
  ["tpl/internal/database.ml"]
  "proj-build/internal/database.ml"

let () = Ml_make.glue
  ["tpl/internal/command_db_create.ml"]
  "proj-build/internal/command_db_create.ml"

let () = make_copy_from_tpl "internal/viewHelpers.ml"

(***********)

let codegen_action () =
  Make.do_make ()

let make_action () =
  codegen_action ();
  sys_command_ok "make -C proj-build -j"

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
