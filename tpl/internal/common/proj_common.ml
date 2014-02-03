open Cd_All
open Strings.Latin1
module IO = IO_Lwt

let ( !! ) = Lazy.force

let ( >>= ) = Lwt.bind
let return = Lwt.return
let fail = Lwt.fail
let catch = Lwt.catch

let return_unit = return ()

module I = Iteratees.Make(IO)

module S = Amall_http_service.Service(IO)(I)

open Amall_http

let respond_gen ?(headers=[]) code reason txt =
  IO.return
  { rs_status_code = code
  ; rs_reason_phrase = reason
  ; rs_headers = { rs_all = headers }
  ; rs_body = Body_string txt
  }

let respond_404 () = respond_gen 404 "Not found" "Not found.\n"

let respond_501 ?(txt = "Not implemented.\n") () =
  respond_gen 501 "Not Implemented" txt

let respond_500 ?(txt = "Internal server error.\n") () =
  respond_gen 500 "Internal server error" txt

let respond_503 ?(txt = "Service unavailable.\n") () =
  respond_gen 500 "Service unavailable" txt

let respond_413 ?(txt = "Request Entity Too Large.\n") () =
  respond_gen 413 "Request Entity Too Large" txt

let respond txt = respond_gen 200 "OK" txt

let respond_renderer layout
  (re : Buffer.t -> unit) = respond begin
  let buf = Buffer.create 1000 in
  let () = layout re buf in
  Buffer.contents buf
end

let response_file path size =
  { rs_status_code = 200
  ; rs_reason_phrase = "OK"
  ; rs_headers =
      { rs_all = []
      }
  ; rs_body = File_contents (path, size)
  }

let respond_file path size =
  IO.return & response_file path size

let redirect_to url =
  respond_gen ~headers:[("Location", url)] 301 "Moved Permanently" ""

module StrMap = Map.Make(String)

let strmap_find_or k default m =
  try StrMap.find k m
  with Not_found -> default

let strmap_find_opt k m =
  try Some (StrMap.find k m)
  with Not_found -> None

module type CONTROLLER_CONTEXT
 =
  sig
    val request : Amall_http.request
    val url_of_path : string -> string
    val params : string StrMap.t
    val redirect_to : string -> Amall_http.response IO.m
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


exception Htfa

let hashtbl_for_all pred h =
  try
    Hashtbl.iter
      (fun k v ->
         if pred k v
         then ()
         else raise Htfa
      )
      h;
    true
  with Htfa -> false

type record_status =
| Rs_new
| Rs_db
| Rs_saved
| Rs_to_delete
| Rs_deleted


exception No_data_found
exception Too_many_rows

let single_of_coll c =
  let v = ref None in
  ( c#iter
      (fun x ->
         match !v with
         | None -> v := Some x
         | Some _ -> raise Too_many_rows
      )
  ; match !v with
    | None -> raise No_data_found
    | Some x -> x
  )

let id_of_string = Int64.of_string
let string_of_id = Int64.to_string

module BitArray = BitArray
