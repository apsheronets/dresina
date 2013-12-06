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
