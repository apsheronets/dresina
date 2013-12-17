
open Combinators

open Printf
open View_helpers
open Amall_http
module IO = IO_Lwt
module I = Iteratees.Make(IO)

open Am_All
open Amall_types
open Lwt

module S = Amall_http_service.Service(IO)(I)

let (my_listener, http_root, _ws_root) =
  S.listener_create (`Inet_any !Init.port)

(* v v v amall kludges v v v *)
(* gds wont' fix that *)
let my_endpoint =
  (http_root, `Fallback [])
(* ^ ^ ^ amall kludges ^ ^ ^ *)

open Http

let my_handler request =
  catch (fun () -> Bindings.f request)
  (function Bindings_lib.Ok r -> r | e -> fail e)

  (*let rec loop = function
    | [] -> return send_404
    | binding::t ->
        try
          let route, dest = binding in
          route#parse dest request
        with Route_lib.Parse_failed -> loop t in
  loop Bindings.l*)

let render_500 =
  let a =
    object
      method title = "error 500: Internal Server Error";
      method content =
        sprintf "<h1>error 500: Internal Server Error</h1>\n<p>Something happend with our webserver.</p><p>Sorry for that.</p>";
    end in
  render (Error.f a)

let send_500 =
  { rs_status_code = 500
  ; rs_reason_phrase = "Internal Server Error"
  ; rs_headers = { rs_all = [] }
  ; rs_body = Body_string render_500
  }

let my_func segpath rq =
  let headers =
    rq.rq_headers.rq_all in
  let (hostname, segpath) =
    match segpath with
    | _ :: hostname :: t -> (hostname, t)
    | _ -> assert false in
  let hostname =
    match !Init.default_host with
    | "" -> hostname
    | s -> s in
  let segpath =
    segpath >>
    List.filter (function "" -> false | _ -> true) >>
    List.map Cd_Strings.Strings.Onebyte.urldecode >>
    (* stupid dtfilter *)
    List.filter (fun x -> not (ExtLib.String.exists x "..")) in
  let params =
    match rq.rq_uri.Uri_type.query with
    | None -> []
    | Some s -> Uri.parse_params s in
  let request = { hostname; segpath; headers } in
  catch (fun () ->
    (* should we send a file from public dir? *)
    match params with
    | [] ->
        (let file_exists path =
          catch
            (fun () ->
              Lwt_unix.lstat path >>= fun stats ->
              match stats.Unix.st_kind with
              | Unix.S_REG -> return true
              | _ -> return false)
            (function _ -> return false) in
        let absolute_path =
          Init.public_dir ^/ (List.fold_left Filename.concat "" segpath) in
        file_exists absolute_path >>= function
        | true -> Http.send_file absolute_path
        | false -> my_handler request)
    | _ -> my_handler request
  )
  (fun e ->
    (* FIXME *)
    Lwt_io.eprintf "ERROR: Responce failed with %s\n" (Printexc.to_string e) >>= fun () ->
    Lwt.return send_500)

let my_func segpath rq =
  I.lift &
    Lwt_unix.with_timeout
      Init.timeout
      (fun () -> my_func segpath rq)

let () = S.mount_http my_endpoint my_func

let () = S.listener_run my_listener
