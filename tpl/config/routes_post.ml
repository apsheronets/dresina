let rec dump_routes ?(indent = 0) level =
  let ind = String.make (4 * indent) ' ' in
  let pr fmt = Printf.(ksprintf (fun s -> printf "%s%s\n%!" ind s) fmt) in
  match level with
  | Action a -> pr "action: %s" a
  | Bind (bind_ident, level) ->
      (pr "bind %s" bind_ident; dump_routes ~indent:(indent + 1) level)
  | Map m ->
      List.iter
        (fun (s, level) ->
           (pr "%S" s; dump_routes ~indent:(indent + 1) level)
        )
        m

let rec generate_level level =
  match level with
  | Action a ->
      Expr.match_ "path"
        [ ("[] | \"\" :: []", a)
        ; ("_", no_route_ml)
        ]
  | Bind (bind_ident, level) ->
      Expr.match_ "path"
        [ ("[]", no_route_ml)
        ; ( sprintf "%s :: path" bind_ident
          , generate_level level
          )
        ]
  | Map m ->
      Expr.match_ "path"
        ( List.map
            (fun (s, level) ->
               ( sprintf "%s :: path" (Lit.string s)
               , generate_level level
               )
            )
            m
        @ [("_", no_route_ml)]
        )

let generate_routes level =
  line_directive "_routes_generated_" 0 ^
  Struc.func "routes" ["path"] (generate_level level)

(******************************************)

let ctx = ref (Map [])

let generate mlt =
  begin
    List.iter
      (function
       | Ml txt -> out txt
       | Dir f -> f ctx
      )
      mlt
  end;
  (* dump_routes !ctx *)
  out (generate_routes !ctx)
