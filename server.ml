(** The Main Module *)

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

let routes request =
  catch (fun () -> Bindings.f request)
  (function Bindings_lib.Ok r -> r | e -> fail e)

let my_func segpath rq =

  let request =
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
    { hostname; segpath; headers; params } in

  (* should we send a file from public dir? *)
  match request.params with
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
      | false -> routes request)
  | _ -> routes request

let my_func segpath rq =
  I.lift &
    catch (fun () ->
      Lwt_unix.with_timeout
        Init.timeout
        (fun () -> my_func segpath rq))
    (fun e ->
      (* FIXME *)
      Lwt_io.eprintf "ERROR: Responce failed with %s\n" (Printexc.to_string e) >>= fun () ->
      Lwt.return send_500)

let () = S.mount_http my_endpoint my_func

let () = S.listener_run my_listener
