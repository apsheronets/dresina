module IO = IO_Lwt

module I = Iteratees.Make(IO)

module S = Amall_http_service.Service(IO)(I)

open Amall_http

(* to make error messages lighter, without iteratee/io/functors *)
module Response
 :
  sig
    type response

    (* [respond_gen http_code http_reason_phrase body] *)
    val respond_gen : int -> string -> string -> response

    (* respond with 200 OK *)
    val respond : string -> response

    val respond_404 : unit -> response

    (**)
    val iteratee_of_response :
      response -> (char, Amall_http.response) I.iteratee
  end
 =
  struct

    type response = (char, Amall_http.response) I.iteratee

    let respond_gen code reason txt =
      I.return
      { rs_status_code = code
      ; rs_reason_phrase = reason
      ; rs_headers = { rs_all = [] }
      ; rs_body = Body_string txt
     }

    let respond_404 () = respond_gen 404 "Not found" "Not found.\n"

    let respond txt = respond_gen 200 "OK" txt

    let iteratee_of_response x = x

  end

include Response

module type CONTROLLER_CONTEXT
 =
  sig
    val request : Amall_http.request
  end
