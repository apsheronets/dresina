
open Combinators

let timeout = 50.

open Printf
open Arg

let port = ref 3000
let default_host = ref ""
let serve_static = ref true

let () =
  let help =
    "FIXME\n" in
  let l = [
    ("-p", Set_int port, sprintf "INT\t\tport to listening; default is %d" !port);
    ("-default-host", Set_string default_host, "STR\tdefault hostname; useful for production environment");
    ("-serve-static", Bool (fun b -> serve_static := b), "BOOL\tserve static pages or not; it's a good idea to disable it on production environments");
  ] in
  Arg.parse l (fun _ -> raise (Arg.Bad help)) help

let public_dir = Filename.dirname Sys.argv.(0) ^/ "public"

