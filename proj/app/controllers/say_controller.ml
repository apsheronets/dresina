let uri_dump = Uri.dump_uri request.rq_uri

let hello () =
  respond & sprintf "Hello.\nRequest: %s\n" uri_dump

let goodbye () =
  respond & sprintf "Goodbye.\nRequest: %s\n" uri_dump
