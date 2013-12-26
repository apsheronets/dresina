let ctx =
  { cur_type = None
  }

let generate mlt =
  out "open Codegen\n";
  begin
    List.iter
      (function
       | Ml txt -> out txt
       | Dir f ->
           f ctx
      )
      mlt
  end;
  finish ctx;
  out begin
    Struc.expr "all_types_tag" &
      Lit.string &
      Printf.sprintf "(module hash %i)" &
      full_hash tags_of_types
  end
