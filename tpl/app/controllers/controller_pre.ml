open Cd_All
open Amall_http
open Proj_common
open Models_views
let sprintf = Printf.sprintf

module Controller (Controller_context : CONTROLLER_CONTEXT) = struct
open Controller_context
