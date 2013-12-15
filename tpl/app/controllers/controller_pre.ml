open Cd_All
open Amall_http
open Proj_common
open Respond_with_view
let sprintf = Printf.sprintf

module Controller (Controller_context : CONTROLLER_CONTEXT) = struct
open Controller_context
