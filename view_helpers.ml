
(** Stuff we will need inside views *)

(** html escaping *)
let esc s =
  let strlen = String.length s in
  let buf = Buffer.create strlen in
  let f = function
    | '&' -> Buffer.add_string buf "&amp;"
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '"' -> Buffer.add_string buf "&quot;"
    |  c  -> Buffer.add_char buf c in
  String.iter f s;
  Buffer.contents buf

(** the same, but outputs to buffer *)
let esc_to_buf buf s =
  let f = function
    | '&' -> Buffer.add_string buf "&amp;"
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '"' -> Buffer.add_string buf "&quot;"
    |  c  -> Buffer.add_char buf c in
  String.iter f s

(** converts ["%D1%85%D1%83%D0%B9"] into ["хуй"] *)
let urldecode = Cd_Strings.Strings.Onebyte.urldecode

(** converts ["хуй"] into ["%D1%85%D1%83%D0%B9"] *)
let urlencode = Cd_Strings.Strings.Onebyte.urlencode

(** [link_to url inner_text] renders [a] tag; no need to escape anything *)
let link_to url inner =
  Printf.sprintf "<a href=\"%s\">%s</a>" (esc url) (esc inner)

open Printf

(** renders view template into string
- [b] is for buffer size; default is [9999] *)
let render ?(b = 9999) f =
  let b = Buffer.create b in
  let () = f b in
  Buffer.contents b

