open Cd_All

let failwith fmt = Printf.ksprintf failwith fmt

let dbg fmt = Printf.ksprintf (fun s -> Printf.eprintf "DBG: %s\n%!" s) fmt

let ( !! ) = Lazy.force

let ( // ) = Filename.concat

let change_suffix ~place fname old_suffix new_suffix =
  if not (Filename.check_suffix fname old_suffix)
  then 
    failwith "%s: expected %s suffix for file %S" place old_suffix fname
  else
    Filename.chop_suffix fname old_suffix ^ new_suffix

let remove_file_if_exists fn =
  if Sys.file_exists fn
  then Sys.remove fn
  else ()

let sys_command cmd =
  dbg "Sys.command: %s" cmd;
  Sys.command cmd

let sys_command_ok cmd =
  let errc = sys_command cmd in
  if errc = 0
  then ()
  else failwith "Command failed, error code %i: %s\n%!" errc cmd

let readdir_list = Sys.readdir @> Array.to_list

let digest_string = Digest.(string @> to_hex)

let digest_string_list l =
  digest_string &
  String.concat "" &
  List.map digest_string l
