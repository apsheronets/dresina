open Cd_All
open Strings.Latin1
open Printf
open Common
module Cg = Codegen

let view_src_dir = "proj/app/views"
let view_dst_dir = "proj-build/app/views"

type view =
  { cntr : string  (* lid *)
  ; view : string  (* lid *)
  ; src : string
  ; suffix : string
  ; make : view -> string (* dst fname *)
  }

let compare_view v1 v2 =
  let c1 = Pervasives.compare v1.cntr v2.cntr in
  if c1 <> 0
  then c1
  else Pervasives.compare v1.view v2.view

let view_func_name ~cntr ~view =
  Cg.Expr.lid (sprintf "%s_%s" cntr view)

let layouts_cntr = "layouts"

let layout_ml_name ~view = "layout_" ^ view ^ ".ml"
and layout_mod_name ~view = "Layout_" ^ view


(* registers rule, returns name of file to be generated *)
let make_html_eml view =
  let (header, dst) =
    if view.cntr = layouts_cntr
    then
      ( "let render (render_template__ : Buffer.t -> unit) (buf : Buffer.t) \
        \ : unit = \
        \  let render_template () : unit = render_template__ buf in begin"
      , view_dst_dir // view.cntr // (layout_ml_name ~view:view.view)
      )
    else
      ( sprintf
          "and %s env (buf : Buffer.t) : unit = begin ignore env; ignore buf;"
          (* ignoring arguments to avoid "unused variable":
             env -- when env is not used to render template,
             buf -- when template is empty.
           *)
          (view_func_name ~cntr:view.cntr ~view:view.view)
      , view_dst_dir // view.cntr // (view.view ^ ".ml.part")
      )
  and footer = "end"
  and src = view.src
  in
  Make.make1 dst [src] begin fun () ->
      sys_command_ok & sprintf
        "ecaml %s -o %s -d -p 'Buffer.add_string buf' \
           -esc-p 'Proj_common.buffer_add_html buf' \
           -header %s -footer %s"
        (Filename.quote src) (Filename.quote dst)
        (Filename.quote header) (Filename.quote footer)
  end;
  dst

(* cntr -> tplname.html.eml -> Some view,
   or None for unknown suffix/build
 *)
let view_desc ~cntr ~fname =
  let suf = ".html.eml" in
  if Filename.check_suffix fname suf
  then begin
    let view = Filename.chop_suffix fname suf in
    Some
      { cntr = Cg.Expr.lid cntr
      ; view = Cg.Expr.lid view
      ; suffix = suf
      ; make = make_html_eml
      ; src = view_src_dir // cntr // fname
      }
  end else begin
    Printf.eprintf "bad view: controller=%S filename=%S\n%!"
      cntr fname;
    None
  end

let views_and_layouts : view list =
  List.sort compare_view &
  List.flatten &
  List.map
    (fun cntr ->
       let cntr_path = view_src_dir // cntr in
       if Sys.is_directory cntr_path
       then
         List.map_filter
           (fun fname ->
              view_desc ~cntr ~fname
           )
           (readdir_list cntr_path)
       else
         []
    )
    (readdir_list view_src_dir)

let (layouts, views) =
  List.partition (fun v -> v.cntr = layouts_cntr) views_and_layouts

let views_dst = List.map (fun v -> v.make v) views

let views_ml = "proj-build/internal/server/views.ml"
and views_modname = "Views"

let () = Make.make1 views_ml views_dst begin fun () ->
  Filew.with_file_out_bin views_ml begin fun out_ch ->
    Ml_comp.copy_ml_to_channel ~out_ch "tpl/internal/common/views_pre.ml";
    List.iter begin fun view_dst ->
        Filew.with_file_in_bin view_dst begin fun in_ch ->
          Filew.copy_channels ~bufsz:60000 in_ch out_ch
        end;
        output_string out_ch "\n"  (* ecaml doesn't add "\n" after footer *)
      end
      views_dst
  end
end

let () =
  let cntr_view_list = List.map (fun v -> (v.cntr, v.view)) views in
  let dep_views = "VIEWS" in
  Hashtbl.add Make.virt_deps dep_views begin
    digest_string_list &
    List.map
      (fun (cntr, view) -> cntr ^ "/" ^ view)
      cntr_view_list
  end;
  let dst = "proj-build/internal/server/respond_with_view.ml" in
  Make.make [dst] ~virtdeps:[dep_views] [] begin fun () ->
    let contents =
      Cg.line_directive "__respond_with_view__" 1 ^
      "open Proj_common\n\n" ^
      let groupped = List.group_pairs ~fst_eq:String.eq cntr_view_list in
      Cg.Struc.items & List.map
        (fun (cntr, views) ->
           Cg.Struc.module_ (String.capitalize cntr) & List.map
             (fun view ->
                Cg.Struc.module_ (String.capitalize view) & List.one &
                Cg.Struc.func "render" ["?(layout=Layout.application)"; "env"] &
                Cg.Expr.call "respond_renderer"
                  [ "layout"
                  ; Cg.Expr.call_gen
                      (Cg.Expr.modqual
                         views_modname
                         (view_func_name ~cntr ~view)
                      )
                      ["env"]
                  ]
             )
             views
        )
        groupped
    in
      Filew.spit_bin dst contents
  end


let layouts_ml_dst = "proj-build/app/views/layout.ml"
let layouts_dst = "proj-build/app/views/layouts"

let () =
  let layouts_dst_mls = List.map (fun v -> v.make v) layouts in
  let dep_layouts = "LAYOUTS" in
  Hashtbl.add Make.virt_deps dep_layouts begin
    digest_string_list layouts_dst_mls
  end;
  Make.make [layouts_ml_dst] [] ~virtdeps:[dep_layouts] begin fun () ->
    Filew.spit_bin layouts_ml_dst begin
      Cg.Struc.items &
      List.map begin fun l ->
          Cg.Struc.expr l.view &
          Cg.Expr.modqual (layout_mod_name ~view:l.view) "render"
        end
        layouts
    end
  end


(* to link this module. *)
let register_make_rules () = ()
