let do_generate ty =
  let outf fmt = Printf.ksprintf out fmt in
  match ty with
  | Record_type (rname, fields) ->
      out begin Record.typedef rname
        (List.map
           (fun f -> (f.f_name, f.f_type))
           fields
        )
      end;
      outf "let ml_of_%s r =\n  \"{ \" ^\n%s ^ \" }\"\n" rname
        (String.concat " ^ \" ; \" ^ \n"
           (List.map
              (fun f -> Printf.sprintf "  \"%s = \" ^ (ml_of_%s r.%s)"
                 f.f_name f.f_type f.f_name
              )
              fields
           )
        )
  | Sum_type (sname, constrs) ->
      out begin Sum.typedef sname
        (List.map
           (fun c -> (c.c_name, c.c_arg_types))
           constrs
        )
      end;
      outf "let ml_of_%s = function\n%s" sname
        (String.concat ""
           (List.map
              (fun c ->
                 let varname i = sprintf "x%i" i in
                 let types = c.c_arg_types in
                 let match_pattern =
                   match List.length types with
                   | 0 -> ""
                   | 1 -> " x"
                   | n ->
                       Printf.sprintf " (%s)" begin
                         let rec mkvars i =
                           if i = n
                           then []
                           else varname i :: mkvars (i + 1)
                         in
                           String.concat ", " (mkvars 0)
                       end
                 in
                 let to_string =
                   let ml_of_ty t x =
                     Expr.call (Printf.sprintf "ml_of_%s" t) [x]
                   in
                   match types with
                   | [] -> ""
                   | [t] -> " ^ " ^ ml_of_ty t "x"
                   | _ -> " ^ \"(\"" ^
                       begin
                         let rec mkvals i types =
                           match types with
                           | [] -> []
                           | t :: types ->
                               ml_of_ty t (varname i)
                               :: mkvals (i + 1) types
                         in
                           String.concat " ^ " (mkvals 0 types)
                       end
                       ^ "\")\""
                 in
                 sprintf "| %s%s -> \"%s \"%s\n"
                   c.c_name match_pattern c.c_name to_string
              )
              constrs
           )
        )
      

let ctx =
  { cur_type = None
  ; finished_type = None
  }

let write_finished_type ctx =
  match ctx.finished_type with
  | None -> ()
  | Some ty ->
      do_generate ty;
      ctx.finished_type <- None

let generate mlt =
  begin
    List.iter
      (function
       | Ml txt -> out txt
       | Dir f ->
           f ctx;
           write_finished_type ctx
      )
      mlt
  end;
  finish ctx;
  write_finished_type ctx
