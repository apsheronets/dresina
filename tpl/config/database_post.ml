let ctx = ref None

let generate mlt =
  List.iter
    (function
     | Ml txt -> out txt
     | Dir f ->
         ( f ctx
         ; match !ctx with
           | None -> ()
           | Some code -> out code
         )
    )
    mlt
