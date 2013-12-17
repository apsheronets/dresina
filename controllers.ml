
open Http
open Lwt
open Combinators
open Printf

let users_controller = object

  method index request =
    return & send_ok_with "index"

  method show request id =
    return & send_ok_with & sprintf "show %S" id

end

let say = object

  method hello request =
    return & send_ok_with "hi"

end

