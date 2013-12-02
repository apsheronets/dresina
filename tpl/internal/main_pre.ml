open Am_All
open Amall_types

module IO = IO_Lwt

module I = Iteratees.Make(IO)

module S = Amall_http_service.Service(IO)(I)

let (my_listener, http_root, _ws_root) = S.listener_create (`Inet_any 4000)

let root_endpoint =
  ( http_root
  , `Service ([], "")
  )

open Amall_http

