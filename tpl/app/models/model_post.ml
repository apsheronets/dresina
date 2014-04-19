let context =
  { cols = None
  ; data_names = Hashtbl.create 13
  }

let generate lst =
  List.iter
    (function
     | Ml txt -> code out_raw txt
     | Dir f -> f context
    )
    lst;
  gather (fun () ->
    out_raw & Opt_string_and_list_cdc_tm.to_string context.cols
  ) ();
  code (fun () ->
    let qout q = Queue.iter (List.one @> out) q in
    List.iter qout [q_opens; q_ctms; q_body]
  ) ()
