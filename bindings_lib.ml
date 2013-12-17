
let bind x y = fun () -> (x, y)

let parse_route request binding =
  let route, dest = binding in
  route#parse dest request

let comb request f1 f2 =
  try
    parse_route request (f1 ())
  with Route_lib.Parse_failed ->
    parse_route request (f2 ())

