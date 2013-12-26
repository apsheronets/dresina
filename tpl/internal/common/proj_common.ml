module IO = IO_Lwt

let ( !! ) = Lazy.force

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
    val url_of_path : string -> string
  end

let url_of_path rq path =
  let open Uri_type in
  let path_add_slash path =
    if String.length path = 0 || path.[0] <> '/'
    then "/" ^ path
    else path
  in
  let scheme_auth scheme_opt authority_opt =
    match scheme_opt, authority_opt with
    | None, _ | Some _, None -> ""
    | Some s, Some a -> s ^ "://" ^
        (match a.userinfo with
         | None -> ""
         | Some u -> u ^ "@"
        ) ^
        (match a.host_kind with
         | IP_literal -> "[" ^ a.host ^ "]"
         | IPv4address | Reg_name -> a.host
        ) ^
        (match a.port with
         | None -> ""
         | Some i -> ":" ^ string_of_int i
        )
  in
  let r = rq.rq_uri in
  match Uri.parse path with
  | None ->
      scheme_auth r.scheme r.authority ^ path_add_slash path
  | Some path_uri ->
      let scheme_opt =
        match path_uri.scheme with
        | (Some _) as s -> s
        | None -> r.scheme
      and authority_opt =
        match path_uri.authority with
        | (Some _) as s -> s
        | None -> r.authority
      in
      scheme_auth scheme_opt authority_opt ^
      path_add_slash path_uri.path ^
      (match path_uri.query with None -> "" | Some q -> "?" ^ q) ^
      (match path_uri.fragment with None -> "" | Some f -> "#" ^ f)

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
