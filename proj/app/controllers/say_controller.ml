let uri_dump = Uri.dump_uri request.rq_uri

let hello () =
  Say.Hello.render & object
    method time = Unix.time ()
    method my_strings = ["abc"; "def"; "ghi"; "jkl"]
    method url = url_of_path Route.Say_controller.Goodbye.path
  end

let goodbye () =
  Say.Goodbye.render empty_env
