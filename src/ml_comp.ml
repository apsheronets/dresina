open Cd_All
open Common

module Cg = Codegen

let sys_command cmd =
  dbg "Sys.command: %s" cmd;
  Sys.command cmd

let copy_ml_to_channel ~out_ch fname =
  Filew.with_file_in_bin
    fname
    (fun in_ch ->
       output_string out_ch & Cg.line_directive fname 1;
       Filew.copy_channels ~bufsz:60000 in_ch out_ch
    )

let copy_mls_to_channel ~out_ch ~files =
  List.iter (copy_ml_to_channel ~out_ch) files

let copy_mls_to_ml ~out ~files =
  Filew.with_file_out_bin
    out
    (fun out_ch ->
       copy_mls_to_channel ~out_ch ~files
    )

let packages_dir pkgs =
  if pkgs = []
  then ""
  else "-package " ^ Filename.quote (String.concat "," pkgs)

let compile_ml_to_byt ?(pkgs = []) ml byt =
  let pkgs = "threads" :: pkgs in
  let cmd = Printf.sprintf
      "ocamlfind ocamlc -g -w A -thread -linkpkg %s %s -o %s"
      (packages_dir pkgs) (Filename.quote ml) (Filename.quote byt) in
  let errc = sys_command cmd
  in
  begin
    if errc <> 0
    then Printf.eprintf "compilation command failed: %s\n%!" cmd
    else ()
  end;
  errc
