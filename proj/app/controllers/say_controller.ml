let uri_dump = Uri.dump_uri request.rq_uri

let hello () =
  Say.Hello.render & object
    method time = Unix.time ()
    method my_strings = ["abc"; "def"; "ghi"; "jkl"]
  end

let goodbye () =
  Say.Goodbye.render empty_env
