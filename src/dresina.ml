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

let () = Ml_make.glue
  [ "tpl/internal/main_pre.ml"
  ; "proj-build/config/routing.ml"
  ; "tpl/internal/main_post.ml"
  ]
  "proj-build/main.ml"

let server_bin = "proj-build/main"

let view_path = "app/views"
let view_emls = ["say/hello.html.eml"]  (* todo: glob *)

let view_bodies =
  List.map begin fun eml ->
      let body_ml = "proj-build" // view_path //
        change_suffix ~place:".eml -> .body.ml" eml ".eml" ".body.ml" in
      let eml = "proj" // view_path // eml in
      Make.make1 body_ml [eml] begin fun () ->
        sys_command_ok & Printf.sprintf
          "ecaml -d %s -o %s -p 'Buffer.add_string buf' \
             -esc-p 'Proj_common.buffer_add_html buf' \
             -header 'begin' -footer '() end'"
          (Filename.quote eml) (Filename.quote body_ml)
        end
    end
    view_emls


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
  server_bin

(***********)

let run_server () =
  Make.do_make ();
  Printf.printf "running server %S...\n%!" server_bin;
  sys_command_ok server_bin

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
