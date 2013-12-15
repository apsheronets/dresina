open Proj_common
open Main_pre

let files_root = "public"

(* takes segments of path, urldecodes them, returns [None] if path after
   urldecoding contains any "upper directory" substring or is not relative.
   Otherwise returns [Some "path/to/something"].
 *)
let urldecode_and_check_path path =
  let open Cd_Strings.Strings.Latin1.String in
  let b = Buffer.create 80 in
  let is_first = ref true in
  List.iter
    (fun seg ->
       if !is_first
       then begin is_first := false end
       else begin Buffer.add_char b '/' end;
       let seg = urldecode seg in
       Buffer.add_string b seg
    )
    path;
  let string = Buffer.contents b in
  if is_prefix ~string ~prefix:"/" ||
     is_prefix ~string ~prefix:"../" ||
     is_suffix ~string ~suffix:"/.." ||
     (find_substring string "/../" <> -1)
  then
    None
  else
    Some string

let io_no_route = IO.error No_route

let serve_file path =
  match urldecode_and_check_path path with
  | None ->
      io_no_route
  | Some path ->
      let path = Filename.concat files_root path in
      (IO.catch
         (fun () -> Lwt_unix.LargeFile.stat path)
         (function _ -> io_no_route)
      ) >>= fun st ->
      let open Unix in let open LargeFile in
      if st.st_kind <> S_REG
      then io_no_route
      else respond_file path st.st_size

let route_not_found () = respond_404 ()

open Amall_http

let http_root_func path rq =
  let path = List.tl path  (* skipping "host:port" part *) in
  I.lift begin
    if rq.rq_method <> `GET
    then
      respond_501 ~txt:"non-GET methods are not implemented.\n" ()
    else
      IO.catch
        (fun () -> serve_file path)
        (function
         | No_route -> begin
             let conctx =
               (module struct
                  let request = rq
                end
                : CONTROLLER_CONTEXT) in
             let route =
               (* there are no IO in "routes", so try-with *)
               try Routing.routes path conctx
               with No_route -> __no_route
             in
             if route == __no_route
             then route_not_found ()
             else route ()
            end
         | e -> IO.error e
        )
  end

let () =
  S.mount_http http_root_endpoint http_root_func;
  S.listener_run my_listener
