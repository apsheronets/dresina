
open Http

exception Parse_failed

let resources (path:string) =
  object
    method index = path
    method show x = path ^ "/" ^ x
    method parse
      (controller:
        < index : Http.request -> Http.response Lwt.t;
          show : Http.request -> string ->
            Http.response Lwt.t >)
      (request:Http.request)
      : Http.response Lwt.t =
      match request.segpath with
      | h :: t when h = path ->
          (match t with
          | [] -> controller#index request
          | [x] -> controller#show request x
          | _ -> raise Parse_failed)
      | _ -> raise Parse_failed
  end

let path (path:string) =
  object
    method path = path
    method parse action (request:Http.request) : Http.response Lwt.t =
      print_endline (String.concat "/" request.segpath);
      if String.concat "/" request.segpath = path
      then action request
      else raise Parse_failed
  end
