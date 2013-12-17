(** Basic types for requests and responses *)

open Combinators
open Amall_http
open Lwt

type http_header = string * string

type request = {
  hostname: string;
  segpath: string list; (** segments of an URL: [/one/two/three] *)
  headers: http_header list; (** incoming http headers *)
  params: (string * string) list; (** query params: [?key=value] *)
}

let rfc822_of_calendar c =
  CalendarLib.Printer.Precise_Calendar.sprint "%a, %d %b %Y %T %z" c

let http_header_of_mtime mtime : http_header =
  let c = CalendarLib.Calendar.Precise.from_unixfloat mtime in
  let rfc822 = rfc822_of_calendar c in
  ("Last-Modified", rfc822)

type response = Amall_http.response

(** high level function; TODO: make a low level one *)
let send_file ?content_type path : response Lwt.t =
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

let send_ok_with ?(headers=[]) ?content_type body : response =
  let headers =
    match content_type with
    | None -> ("Content-Type", "text/html") :: headers
    | Some s -> ("Content-Type", s) :: headers in
  { rs_status_code = 200
  ; rs_reason_phrase = "OK"
  ; rs_headers = { rs_all = headers }
  ; rs_body = Body_string body
  }

open View_helpers
open Printf

let render_404 =
  let a =
    object
      method title = "error 404: not found";
      method content =
        "<h1>error 404: not found</h1>\n<p>no such page</p>";
    end in
  render (Error.f a)

let send_404 =
  { rs_status_code = 404
  ; rs_reason_phrase = "Not found"
  ; rs_headers = { rs_all = [] }
  ; rs_body = Body_string render_404
  }

let render_500 =
  let a =
    object
      method title = "error 500: Internal Server Error";
      method content =
        sprintf "<h1>error 500: Internal Server Error</h1>\n<p>Something happened with our webserver.</p><p>Sorry for that.</p>";
    end in
  render (Error.f a)

let send_500 =
  { rs_status_code = 500
  ; rs_reason_phrase = "Internal Server Error"
  ; rs_headers = { rs_all = [] }
  ; rs_body = Body_string render_500
  }


