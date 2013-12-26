open Proj_common

let link_to txt url =
  let buf = Buffer.create 100 in
  Printf.bprintf buf "<a href=\"%a\">%a</a>"
    buffer_add_html url
    buffer_add_html txt;
  Buffer.contents buf
