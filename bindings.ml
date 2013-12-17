
open Lwt
open Bindings_lib

let f : Http.request -> Http.response Lwt.t =

  bind Routes.say_hello Controllers.say#hello >>=
  bind Routes.users Controllers.users_controller >>=
  otherwise_send_404

