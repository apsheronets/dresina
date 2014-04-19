let ctx = ref None

let generate mlt =
  List.iter
    (function
     | Ml txt -> out_raw txt
     | Dir f ->
         ( f ctx
         ; match !ctx with
           | None -> ()
           | Some code -> out_raw code
         )
    )
    mlt
