type field =
  { f_name : string
  ; f_type : string
  }

type sum_constr =
  { c_name : string
  ; c_arg_types : string list
  }

type ty =
| Record_type of string * field list
| Sum_type of string * sum_constr list
| Alias of string * string


let tags_of_types = ref []

let full_hash x = Hashtbl.hash_param max_int max_int x

let do_generate ty =
  let outf fmt = Printf.ksprintf out fmt in
  let () =
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
                     | _ -> " ^ \"(\" ^ " ^
                         begin
                           let rec mkvals i types =
                             match types with
                             | [] -> []
                             | t :: types ->
                                 ml_of_ty t (varname i)
                                 :: mkvals (i + 1) types
                           in
                             String.concat " ^ \", \" ^ " (mkvals 0 types)
                         end
                         ^ " ^ \")\""
                   in
                   sprintf "| %s%s -> \"%s \"%s\n"
                     c.c_name match_pattern c.c_name to_string
                )
                constrs
             )
          )
    | Alias (newname, oldname) ->
        outf "type %s = %s;;\n" newname oldname;
        outf "let ml_of_%s = ml_of_%s;;\n" newname oldname
  in
  let () =
    let hash = full_hash ty in
    let type_name =
      match ty with
      | Record_type (rname, _) -> rname
      | Sum_type (sname, _) -> sname
      | Alias (newname, _) -> newname
    in
    let tag = Printf.sprintf "%s (typedef hash %i)" type_name hash in
    let () = tags_of_types := tag :: !tags_of_types in
    let tag_ident = type_name ^ "_tag" in

    outf "let %s = %s;;\n" tag_ident (Lit.string tag);

    outf "let %s_to_file ?(flags = []) fname (v : %s) =\n\
         \  Tagged_marshal.to_file ~flags ~tag:%s fname v;;\n"
      type_name type_name tag_ident;

    outf "let %s_from_file fname : %s =\n\
         \  Tagged_marshal.from_file ~tag:%s fname;;\n"
      type_name type_name tag_ident

  in
    ()



type context =
  { mutable cur_type : ty option
  }

let finish ctx =
  match ctx.cur_type with
  | None -> ()
  | Some ty ->
      ctx.cur_type <- None;
      do_generate ty

let flush0 = finish

let record_type1 rname ctx =
  finish ctx;
  ctx.cur_type <- Some (Record_type (rname, []))

let field2 fname ftype ctx =
  match ctx.cur_type with
  | None | Some (Sum_type _) | Some (Alias _) ->
      failwith "field definition must be in record definition only"
  | Some (Record_type (rname, fields)) ->
      let fields = fields @ [ { f_name = fname ; f_type = ftype } ] in
      ctx.cur_type <- Some (Record_type (rname, fields))

let sum_type1 sname ctx =
  finish ctx;
  ctx.cur_type <- Some (Sum_type (sname, []))

let cur_sum_type ctx =
  match ctx.cur_type with
  | None
  | Some (Record_type _) | Some (Alias _) ->
      failwith "currently-constructed type is not a sum-type"
  | Some (Sum_type (sname, st)) -> (sname, st)

let sum_add_constr cname st =
  st @ [ { c_name = cname ; c_arg_types = [] } ]

let sum_constr1 cname ctx =
  let (sname, st) = cur_sum_type ctx in
  let st = sum_add_constr cname st in
  ctx.cur_type <- Some (Sum_type (sname, st))

let sum_add_arg atype st =
  match st with
  | [] -> failwith "sum type arguments are added inside sum type constructors"
  | _ ->
      let rec loop st =
        match st with
        | [] -> assert false
        | [h'] -> [ { (h') with c_arg_types = h'.c_arg_types @ [atype] } ]
        | h' :: t' -> h' :: loop t'
      in
        loop st

let sum_arg1 atype ctx =
  let (sname, st) = cur_sum_type ctx in
  let st = sum_add_arg atype st in
  ctx.cur_type <- Some (Sum_type (sname, st))

let alias2 newname oldname ctx =
  finish ctx;
  check_lid ~place:"type_gen/alias/oldname" oldname;
  check_lid ~place:"type_gen/alias/newname" newname;
  ctx.cur_type <- Some (Alias (newname, oldname))
