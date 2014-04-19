let ctx =
  { cur_type = None
  }

let generate mlt =
  out [Struc.open_ ["Codegen"; "Cg2"]];
  begin
    List.iter
      (function
       | Ml txt -> out_raw txt
       | Dir f -> f ctx
      )
      mlt
  end;
  finish ctx;
  out & List.one &
    Struc.expr "all_types_tag" &
      Expr.string &
      Printf.sprintf "(module hash %i)" &
      full_hash tags_of_types
