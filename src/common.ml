let failwith fmt = Printf.ksprintf failwith fmt

let dbg fmt = Printf.ksprintf (fun s -> Printf.eprintf "DBG: %s\n%!" s) fmt

let ( !! ) = Lazy.force
