
open Combinators
open Amall_http
open Lwt

type http_header = string * string

type request = {
  hostname: string;
  segpath: string list;
  headers: http_header list;
}

let rfc822_of_calendar c =
  CalendarLib.Printer.Precise_Calendar.sprint "%a, %d %b %Y %T %z" c

let http_header_of_mtime mtime : http_header =
  let c = CalendarLib.Calendar.Precise.from_unixfloat mtime in
  let rfc822 = rfc822_of_calendar c in
  ("Last-Modified", rfc822)


(* high level *)
let send_file ?content_type path =
  let rs_all = [] in
  Lwt_unix.lstat path >>= fun stats ->
  let mtime = stats.Unix.st_mtime in
  let rs_all = http_header_of_mtime mtime :: rs_all in
  let rs_all =
    match content_type with
    | None -> rs_all
    | Some s -> ("Content-Type", s) :: rs_all in
  let size = Int64.of_int & stats.Unix.st_size in
  return
  { rs_status_code = 200
  ; rs_reason_phrase = "OK"
  ; rs_headers = { rs_all }
  ; rs_body = File_contents (path, size)
  }

