open Cd_All
open Common
open Ml_comp
open Printf

let includes incl =
  String.concat " " &
  List.flatten &
  List.map (fun inc_dir -> ["-I"; Filename.quote inc_dir]) &
  incl

let glue mls out =
  Make.make1 out mls begin fun () ->
    Ml_comp.copy_mls_to_ml ~out ~files:mls
  end

let wrap_in_module ~m src dst =
  Codegen.check_uid ~place:"Ml_make.wrap_in_module" m;
  Make.make1 dst [src] begin fun () ->
    Filew.with_file_out_bin dst begin fun out_ch ->
      Printf.fprintf out_ch "module %s = struct\n" m;
      Ml_comp.copy_ml_to_channel ~out_ch src;
      output_string out_ch "end;;\n"
    end
  end

let compile_objs_to_nat ?(pkgs = []) ?(incl = []) objs nat =
  Make.make1 nat objs begin fun () ->
    sys_command_ok & sprintf 
      "ocamlfind ocamlopt -g %s %s -linkpkg -o %s %s"
        (includes incl)
        (packages_dir ~pkgs)
        nat
        (String.concat " " &
         List.map Filename.quote objs
        )
  end

(* returns cmx name *)
let compile_ml_to_obj ?(pkgs = []) ?(incl = []) ~deps ml =
  let suff new_suff = change_suffix ~place:"compile_ml" ml ".ml" new_suff in
  let (cmx, cmi, o) = Tuple3.map_mono suff (".cmx", ".cmi", ".o") in
  let clean_files = [cmi; o] in
  Make.make1 ~clean_files cmx (ml :: deps) begin fun () ->
    sys_command_ok & sprintf
      "ocamlfind ocamlopt -g -w A %s %s -c %s"
      (includes incl)
      (packages_dir ~pkgs)
      (Filename.quote ml)
  end;
  cmx

let compile_mls_to_nat ?(pkgs = []) ?(pre_objs = []) ?(incl = []) mls nat =
  let mls = List.map (fun ml -> "proj-build" // ml) mls in
  let cmxes =
    let rec loop cmxes deps mls =
      match mls with
      | [] -> List.rev cmxes
      | ml :: mls ->
         let cmx = compile_ml_to_obj ~pkgs ~deps ~incl ml in
         loop (cmx :: cmxes) (cmx :: deps) mls
    in
      loop [] pre_objs mls
  in
  compile_objs_to_nat ~pkgs ~incl (pre_objs @ cmxes) nat
