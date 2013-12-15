module IO = IO_Lwt

let ( >>= ) = Lwt.bind

module I = Iteratees.Make(IO)

module S = Amall_http_service.Service(IO)(I)

open Amall_http

let respond_gen code reason txt =
  IO.return
  { rs_status_code = code
  ; rs_reason_phrase = reason
  ; rs_headers = { rs_all = [] }
  ; rs_body = Body_string txt
  }

let respond_404 () = respond_gen 404 "Not found" "Not found.\n"

let respond_501 ?(txt = "Not implemented.\n") () =
  respond_gen 501 "Not Implemented" txt

let respond txt = respond_gen 200 "OK" txt

let respond_renderer layout
  (re : Buffer.t -> unit) = respond begin
  let buf = Buffer.create 1000 in
  let () = layout re buf in
  Buffer.contents buf
end

let respond_file path size =
  IO.return
    { rs_status_code = 200
    ; rs_reason_phrase = "OK"
    ; rs_headers =
        { rs_all = []
        }
    ; rs_body = File_contents (path, size)
    }

module type CONTROLLER_CONTEXT
 =
  sig
    val request : Amall_http.request
  end

let __no_route : unit -> Amall_http.response IO.m =
  fun () -> failwith "dresina: internal error: no route"

let sprintf fmt = Printf.sprintf fmt

let buffer_add_html buf s =
  let imax = String.length s - 1 in
  for i = 0 to imax do
    let c = s.[i] in
    match c with
    | '&' -> Buffer.add_string buf "&amp;"
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '"' -> Buffer.add_string buf "&quot;"
    |  c  -> Buffer.add_char buf c
  done

let empty_env = object end
