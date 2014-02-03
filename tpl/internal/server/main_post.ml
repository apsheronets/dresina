open Cd_All
open Strings.Latin1
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

let io_no_file = IO.return `No_file

let serve_file path =
  match urldecode_and_check_path path with
  | None ->
      io_no_file
  | Some path ->
      let path = Filename.concat files_root path in
      (IO.catch
         (fun () ->
            Lwt_unix.LargeFile.stat path >>= fun st ->
            IO.return (`Stats st)
         )
         (function _ -> io_no_file)
      ) >>= begin function
      | `No_file -> io_no_file
      | `Stats st ->
          let open Unix in let open LargeFile in
          if st.st_kind <> S_REG
          then io_no_file
          else IO.return & `File_response (response_file path st.st_size)
      end

let route_not_found () = respond_404 ()

open Amall_http
open I.Ops

let empty_params = StrMap.empty

let max_form_size = 100_000

let it_params_of_form_urlencoded () =
  I.limit max_form_size I.gather_to_string >>= fun it ->
  I.return &
  match it with
  | I.IE_cont ((Some e), _k) -> `Error e
  | I.IE_cont (None, _k) -> `Too_large
  | I.IE_done str -> `Ok begin
    (* todo: without split (lexing maybe?) *)
    str
    |> String.split_exact ((=) '&')
    |> List.fold_left
         (fun acc binding ->
            if binding = ""
            then acc
            else
              let (var_key, eq_sign, var_val) =
                String.split_by_first ((=) '=') binding
              in
                let key = String.urldecode var_key in
                StrMap.add
                  key
                  (if eq_sign = "" then "" else String.urldecode var_val)
                  acc
         )
         empty_params
    end

let it_empty_params = I.return & `Ok empty_params

let http_root_func path rq =
  let path = List.tl path  (* skipping "host:port" part *) in
  begin match List.Assoc.get_opt ~keq:String.eq_nocase_latin1
         "Content-type" rq.rq_headers.rq_all with
  | None -> it_empty_params
  | Some ct ->
      if String.eq_nocase_latin1 ct "application/x-www-form-urlencoded"
      then it_params_of_form_urlencoded ()
      else it_empty_params
  end
  >>= fun params_res ->
  begin match params_res with
  | `Error e -> I.lift & respond_503 ~txt:(Printexc.to_string e) ()
  | `Too_large -> I.lift & respond_413 ()
  | `Ok params ->
  I.lift begin
    match rq.rq_method with
    | (`GET | `POST) as meth ->
      begin serve_file path >>% function
      | `File_response fr -> IO.return fr
      | `No_file ->
          let conctx =
            (module struct
               let request = rq
               let url_of_path = Proj_common.url_of_path rq
               let params = params
               let redirect_to path = Proj_common.redirect_to
                 (url_of_path path)
             end
             : CONTROLLER_CONTEXT) in
          let route =
            (* there are no IO in "routes", so try-with *)
            try Routing.routes meth path conctx
                (* todo: переделать так, чтобы conctx не применялся тут,
                   а роутинг возвращал функцию, требующую conctx.
                   и также кое-что ещё -- max_request_body_size,
                   например.  на основании которого и лепить
                   conctx.
                 *)
            with No_route -> __no_route
          in
          if route == __no_route
          then route_not_found ()
          else
            IO.catch
              route
              (fun e ->
                 (* todo: сделать это только для dev-окружения *)
                 let txt = sprintf
                   "Uncaught exception: %s\nBacktrace:\n%s\n"
                     (Printexc.to_string e)
                     (Printexc.get_backtrace ())
                 in
                 respond_500 ~txt ()
              )
      end
    | `HEAD ->
      respond_501 ~txt:"only GET and POST methods are implemented.\n" ()
  end
  end

let server () =
  Database.check_schema_version ();
  S.mount_http http_root_endpoint http_root_func;
  S.listener_run my_listener

let usage () =
  print_string "\
usage:\n\
\  -h | --help | help  --  this message\n\
\  without arguments or with \"server\" command  --  run web server\n\
\  db:create  --  create database based on this project's database configuration\n\
\  db:drop  --  drop project's database (you'll lose schema and data both)\n\
\  db:migrate [up_to_id]  --  apply migrations to database (all pending, or\n\
\                             only up to 'up_to_id' inclusive)\n\
\  db:rollback  --  roll back last applied migration\n\
"

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [] | "server" :: _ -> server ()
  | ("--help" | "-h" | "help") :: _ -> usage ()
  | "db:create" :: _ -> Command_db_create.db_create ()
  | "db:drop" :: _ -> Command_db_drop.db_drop ()
  | "db:migrate" :: args ->
      let up_to =
        match args with
        | [] -> None
        | id :: [] -> Some id
        | _ -> failwith "command db:migrate can have only one argument"
      in
        Command_db_migrate.db_migrate ?up_to ()
  | "db:rollback" :: _ -> Command_db_rollback.db_rollback ()
  | _ ->
      Printf.eprintf "%s: invalid command line\n%!" Sys.argv.(0);
      usage ()
