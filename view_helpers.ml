
(* html escaping *)
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

let esc_to_buf buf s =
  let f = function
    | '&' -> Buffer.add_string buf "&amp;"
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '"' -> Buffer.add_string buf "&quot;"
    |  c  -> Buffer.add_char buf c in
  String.iter f s

