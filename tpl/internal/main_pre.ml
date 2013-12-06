open Am_All
open Amall_types
open Proj_common

let (my_listener, http_root, _ws_root) = S.listener_create (`Inet_any 4000)

let http_root_endpoint =
  ( http_root
  , `Service ([], "")
  )

open Amall_http

exception No_route
