let context =
  { cols = None
  ; fetchers_names = Hashtbl.create 13
  }

let generate lst =
  List.iter
    (function
     | Ml txt -> code out txt
     | Dir f -> f context
    )
    lst;
  gather (fun () ->
    out & Opt_list_cdc_tm.to_string context.cols
  ) ()
