let context =
  { cols = None
  ; data_names = Hashtbl.create 13
  }

let generate lst =
  List.iter
    (function
     | Ml txt -> code out txt
     | Dir f -> f context
    )
    lst;
  gather (fun () ->
    out & Opt_string_and_list_cdc_tm.to_string context.cols
  ) ()
