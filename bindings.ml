
open Bindings_lib

let f request =
  let (>>=) = comb request in

  bind Routes.say_hello Controllers.say#hello >>=
  bind Routes.users Controllers.users_controller

