
open Lwt

let bind route dest (request:Http.request) =
  route#parse dest request

let otherwise_send_404 =
  bind (object method parse action _ = action end) (return Http.send_404)

exception Ok of Http.response Lwt.t

let (>>=) f1 f2 (request:Http.request) =
  try
    let r = f1 request in
    raise (Ok r)
  with Route_lib.Parse_failed -> f2 request

