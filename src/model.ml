open Cd_All
open Strings.Latin1
open Printf
open Common
module Cg = Codegen

let model_src_dir = "proj/app/models"
let model_dst_dir = "proj-build/app/models"

let mlt_basename n =
  if Filename.check_suffix n ".mlt"
  then
    Some (Filename.chop_suffix (Filename.basename n) ".mlt")
  else
    None

type model =
  { fname : string
  ; base : string
  ; desc : string
  ; code : string
  }

let models = model_src_dir |> readdir_list |>
  List.map_filter
    (fun fname ->
       match mlt_basename fname with
       | None -> None
       | Some base ->
           let fname = model_src_dir // fname in
           let base = String.uncapitalize base in
           let () =
             Cg.check_lid ~place:(Printf.sprintf "model %S" fname) base in
           Some
             { fname = fname
             ; base = base
             ; desc = model_dst_dir // (base ^ ".desc.bin")
             ; code = model_dst_dir // ("model_" ^ base ^ ".ml")
             }
    )

let stage_gather_ml = "tpl/app/models/stage_gather.ml.part"
let stage_code_ml = "tpl/app/models/stage_code.ml.part"

let sql_lexer_ml = "proj-build/internal/common/sql_lexer.ml"

let gather_deps_list = sql_lexer_ml :: Migration.schema_targets

(*
  сами описания пре-кусок будет скидывать в файлики, типа
  "имя_модели.desc.bin".
*)

let model_common_pre =
  [ "proj-build/internal/common/tagged_marshal.ml.module"
  ; "proj-build/db/migrate/migrate_types.ml.module"
  ; "proj-build/internal/common/schema_types.ml.module"
  ; "proj-build/internal/common/sql_lexer.ml.module"
  ; "tpl/app/models/model_pre.ml"
  ]

let model_stage ~stage_ml ~add_deps ~target m =
  Staging.stage
    ~mlt:m.fname
    ~pre:(stage_ml :: model_common_pre)
    ~post:["tpl/app/models/model_post.ml"]
    ~add_deps:(stage_ml :: add_deps)
    target

let all_descs = List.map (fun m -> m.desc) models

let () = List.iter
  (fun m ->
     model_stage ~stage_ml:stage_gather_ml ~add_deps:gather_deps_list
       ~target:m.desc m;
     model_stage ~stage_ml:stage_code_ml ~add_deps:all_descs
       ~target:m.code m
  )
  models

(*
  а для второй стадии сгенерить models_stage2.ml из
  "stage = `Stage2 of описание_столбцов".
*)

let dep_models = "MODELS"

let () =
  Hashtbl.add Make.virt_deps dep_models begin
    digest_string_list &
    List.map
      (fun m -> m.base)
      models
  end

let register_make_rules () = ()
