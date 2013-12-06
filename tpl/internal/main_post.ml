let serve_file _path = raise No_route

let route_not_found () = iteratee_of_response (respond_404 ())

let http_root_func path rq =
  if rq.rq_method <> `GET
  then route_not_found ()
  else
  try
    serve_file path
  with
  | No_route ->
  let conctx = (module struct
      let request = rq
    end : CONTROLLER_CONTEXT) in
  let path = List.tl path in
  try
    iteratee_of_response (routes path conctx)
  with
  | No_route -> route_not_found ()

let () =
  S.mount_http http_root_endpoint http_root_func;
  S.listener_run my_listener
