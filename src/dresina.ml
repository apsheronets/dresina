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
  "database.ml"

(*
let () = compile_byt ~pkgs:dbpkgs "proj-build/config/database.ml"
*)

let () = stage_multi_paths
  ~pkgs:webpkgs
  ~rel_path:"config"
  ~pre:["routes_pre.ml"]
  ~post:["routes_post.ml"]
  ~mlt:"routes.mlt"
  [("routes", "route.ml"); ("routing", "routing.ml")]

let () = Make.do_make ()


(***********)

let run_server () = print_string "run server\n"

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

let cmds = [server_cmd; clean_cmd; dump_deps_cmd]

let () = match Term.eval_choice default_cmd cmds with
| `Error _ -> exit 1
| `Ok action -> ignore (action ()); exit 0
| `Help | `Version -> exit 0
