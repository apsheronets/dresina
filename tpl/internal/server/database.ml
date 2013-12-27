open Proj_common
open Dbi
open Dbi_pg

type conn_pool_info =
  { conn_info : unit -> conn_info
  ; schema : string option
  ; pool : int
  }

let ref_conn_pool_info = ref None

let conn_pool_info = lazy begin
  match !ref_conn_pool_info with
  | None -> failwith "no database connection configured"
  | Some i -> i
end

let check_not_contains ~desc_char ~ch ~desc_str ~str =
  match str with
  | None -> ()
  | Some str ->
      if String.contains str ch
      then failwith
        (sprintf "database configuration error: ~%s can't contain %s"
           desc_str desc_char)
      else ()

let check_no_quot = check_not_contains ~desc_char:"double quotes" ~ch:'"'
let check_no_apos = check_not_contains ~desc_char:"apostrophes" ~ch:'\''

let register ?host ?port ?dbname ?user ?password
  ?options ?requiressl ?tty
  ?conninfo
  ?(pool = 5)
  ?schema
  ()
 =
  check_no_quot ~str:user ~desc_str:"user";
  check_no_apos ~str:password ~desc_str:"password";
  check_no_quot ~str:dbname ~desc_str:"dbname";
  let schema =
    match schema with
    | None -> user
    | Some _ -> schema
  in
  check_no_quot ~str:schema ~desc_str:"schema";
  ref_conn_pool_info := Some
    { conn_info = begin fun () -> ((new conn_info
        ?host ?port ?dbname ?user ?password
        ?options ?requiressl ?tty
        ?conninfo
        ()   ) : conn_info)
      end
    ; pool = pool
    ; schema = schema
    }

let open_connection () =
  let conn = new connection ((!!conn_pool_info).conn_info ()) in
  begin match (!!conn_pool_info).schema with
  | None -> ()
  | Some s ->
      let (_ : Dbi_pg.result) = conn#execute (sprintf
        "set search_path = \"%s\", public"
        s
        )
      in ()
  end;
  conn

let pool = lazy begin
  Lwt_pool.create
    (!!conn_pool_info).pool
    (fun () -> Lwt_preemptive.detach open_connection ()
    )
end

let with_connection f =
  Lwt_pool.use !!pool
    (fun conn ->
       Lwt_preemptive.detach
         (fun c -> try `Ok (f c) with e -> `Error e)
         conn
    )
  >>= fun res ->
  match res with
  | `Ok r -> IO.return r
  | `Error e -> IO.error e

(* use connection in current thread, blocking other lwt threads;
   close connection after use.
   Planned usage: only for pre-server functions like "checking schema
   version" or 'db:migrate'.
 *)
let with_connection_blocking f =
  let conn = open_connection () in
  let finally () = conn#disconnect () in
  try
    let r = f conn in
    finally ();
    r
  with
  | e -> finally (); raise e
