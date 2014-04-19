open Cd_All
open Printf

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
| Tuple_type of string * string list

let tags_of_types = ref []

let full_hash x = Hashtbl.hash_param max_int max_int x

let ml_of_ t = "ml_of_" ^ t

let expr_of_ t = "expr_of_" ^ t

let cat exprlist =
  Expr.(call_mod ["String"] "concat" [string ""; exprlist])

let do_generate ty =
  (* let outf fmt = Printf.ksprintf out fmt in *)
  let () = out & List.one &
    match ty with
    | Record_type (rname, fields) ->
        Struc.type_
          (Typ.prim rname)
          (Typ.record &
           List.map
             (fun f -> (f.f_name, Typ.prim f.f_type))
             fields
          )
    | Sum_type (sname, constrs) ->
        Struc.type_
          (Typ.prim sname)
          (Typ.sum &
           List.map
             (fun c -> (c.c_name, List.map Typ.prim c.c_arg_types))
             constrs
          )
    | Alias (newname, oldname) ->
        Struc.type_ (Typ.prim newname) (Typ.prim oldname)
    | Tuple_type (t, components) ->
        assert (List.length components >= 2);
        Struc.type_ (Typ.prim t) (Typ.tuple & List.map Typ.prim components)
  in
  let _q () = out &
    (* legacy *)
    match ty with
    | Record_type (rname, fields) ->
        [ Struc.type_
            (Typ.prim rname)
            (Typ.record &
             List.map
               (fun f -> (f.f_name, Typ.prim f.f_type))
               fields
            )
        ; Struc.func (ml_of_ rname) [Patt.lid "r"] &
            let open Expr in
            cat & list &
              string "{" ::
              List.concat_with
                [ string "; " ]
                (List.map
                   (fun f ->
                      let open Expr in
                      [call (ml_of_ f.f_type) [ field (lid "r") f.f_name ]]
                   )
                   fields
                )
            @ [string "}"]
        ]
    | Sum_type (sname, constrs) ->
        [ Struc.type_
            (Typ.prim sname)
            (Typ.sum &
             List.map
               (fun c -> (c.c_name, List.map Typ.prim c.c_arg_types))
               constrs
            )
        ; Struc.func (ml_of_ sname) [Patt.lid "s"] &
            let open Expr in
            cat &
            Expr.match_ (lid "s") &
            List.map
              (fun c ->
                 let varname i = sprintf "x%i" i in
                 let args = List.mapi (fun i t -> varname i, t) c.c_arg_types
                 in
                 ( Patt.constr c.c_name &
                     List.map (fst @> Patt.lid) args
                 , list &
                   if args = [] then [] else [string "("]
                   @
                   [string c.c_name]
                   @
                   if args = []
                   then []
                   else begin
                     string " "
                     ::
                     let vals =
                       List.map
                         (fun (x, t) ->
                            [call (ml_of_ t) [lid x]]
                         )
                         args
                     in
                     if List.length vals = 1
                     then
                       List.flatten vals
                     else
                       string "("
                       ::
                       List.concat_with [string ", "] vals
                       @
                       [string ")"]
                   end
                 @
                   if args = [] then [] else [string ")"]
                 )  (* match case *)
              )
              constrs
        ]
    | Alias (newname, oldname) ->
        [ Struc.type_ (Typ.prim newname) (Typ.prim oldname)
        ; Struc.expr (ml_of_ newname) & Expr.lid (ml_of_ oldname)
        ]
    | Tuple_type (t, components) ->
        assert (List.length components >= 2);
        let xs = List.mapi (fun i c -> sprintf "x%i" i, c) components in
        [ Struc.type_ (Typ.prim t) (Typ.tuple & List.map Typ.prim components)
        ; Struc.func (ml_of_ t)
            [Patt.tuple & List.map (fst @> Patt.lid) xs] &
            let open Expr in
            cat & list &
              string "(" ::
              List.concat_with
                [ string ", " ]
                (List.map
                  (fun (x, t) -> [Expr.call (ml_of_ t) [lid x]])
                  xs
                )
            @ [string ")"]
        ]
  in
  let () = out &
    let expr' l args =
      let open Expr in
      call_mod ["Expr"] l args
    in
    let expr1' l arg = expr' l [arg] in
    match ty with
    | Record_type (rname, fields) ->
        [ Struc.func
          (expr_of_ rname) ~ret_typ:(Typ.prim "expr") [Patt.lid "r"] &
            let open Expr in
            expr1' "record" & list &
              List.map
                (fun f ->
                   tuple &
                     [ string f.f_name
                     ; call (expr_of_ f.f_type) [ field (lid "r") f.f_name ]
                     ]
                )
                fields
        ]
    | Sum_type (sname, constrs) ->
        [ Struc.func
          (expr_of_ sname) ~ret_typ:(Typ.prim "expr") [Patt.lid "s"] &
            let open Expr in
            Expr.match_ (lid "s") &
            List.map
              (fun c ->
                 let varname i = sprintf "x%i" i in
                 let args = List.mapi (fun i t -> varname i, t) c.c_arg_types
                 in
                 ( Patt.constr c.c_name &
                     List.map (fst @> Patt.lid) args
                 , expr' "constr" &
                   [ string c.c_name
                   ; Expr.list &
                       List.map
                         (fun (x, t) ->
                            call (expr_of_ t) [lid x]
                         )
                         args
                   ]
                 )
              )
              constrs
        ]
    | Alias (newname, oldname) ->
        [ Struc.expr (expr_of_ newname) & Expr.lid (expr_of_ oldname)
        ]
    | Tuple_type (t, components) ->
        assert (List.length components >= 2);
        let xs = List.mapi (fun i c -> sprintf "x%i" i, c) components in
        [ Struc.func (expr_of_ t)
            [Patt.tuple & List.map (fst @> Patt.lid) xs] &
            let open Expr in
            expr1' "tuple" &
            list & List.map
              (fun (x, t) -> Expr.call (expr_of_ t) [lid x])
              xs
        ]
  in
  let () =
    let hash = full_hash ty in
    let type_name =
      match ty with
      | Record_type (rname, _) -> rname
      | Sum_type (sname, _) -> sname
      | Alias (newname, _) -> newname
      | Tuple_type (tname, _) -> tname
    in
    let tag = Printf.sprintf "%s (typedef hash %i)" type_name hash in
    let () = tags_of_types := tag :: !tags_of_types in
    let tag_ident = type_name ^ "_tag" in
    out
    [
      Struc.expr tag_ident (Expr.string tag)
    ;
      Struc.let_
        (Patt.func (type_name ^ "_to_file")
           [ Patt.opt "flags" ~def:(Expr.list [])
           ; Patt.lid "fname"
           ; Patt.ascribe (Patt.lid "v") (Typ.prim type_name)
           ]
        )
        (Expr.call_mod ["Tagged_marshal"] "to_file"
           ~lab:[Arg.lab_self "flags"; Arg.lab "tag" (Expr.lid tag_ident)]
           [Expr.lid "fname"; Expr.lid "v"]
        )
    ;
      Struc.func
        (type_name ^ "_from_file")
        [Patt.lid "fname"]
        ~ret_typ:(Typ.prim type_name) &
          Expr.call_mod ["Tagged_marshal"] "from_file"
            ~lab:[Arg.lab "tag" (Expr.lid tag_ident)]
            [Expr.lid "fname"]
    ]
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

open Mlt

let record_type1 = string_args1 & fun rname ctx ->
  finish ctx;
  ctx.cur_type <- Some (Record_type (rname, []))

let field2 = string_args2 & fun fname ftype ctx ->
  match ctx.cur_type with
  | None | Some (Sum_type _ | Alias _ | Tuple_type _) ->
      failwith "field definition must be in record definition only"
  | Some (Record_type (rname, fields)) ->
      let fields = fields @ [ { f_name = fname ; f_type = ftype } ] in
      ctx.cur_type <- Some (Record_type (rname, fields))

let sum_type1 = string_args1 & fun sname ctx ->
  finish ctx;
  ctx.cur_type <- Some (Sum_type (sname, []))

let cur_sum_type ctx =
  match ctx.cur_type with
  | None
  | Some (Record_type _ | Alias _ | Tuple_type _) ->
      failwith "currently-constructed type is not a sum-type"
  | Some (Sum_type (sname, st)) -> (sname, st)

let sum_add_constr cname st =
  st @ [ { c_name = cname ; c_arg_types = [] } ]

let sum_constr1 = string_args1 & fun cname ctx ->
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

let sum_arg1 = string_args1 & fun atype ctx ->
  let (sname, st) = cur_sum_type ctx in
  let st = sum_add_arg atype st in
  ctx.cur_type <- Some (Sum_type (sname, st))

let alias2 = string_args2 & fun newname oldname ctx ->
  finish ctx;
  check_lid ~place:"type_gen/alias/oldname" oldname;
  check_lid ~place:"type_gen/alias/newname" newname;
  ctx.cur_type <- Some (Alias (newname, oldname))

let tuple_type1 = string_args1 & fun tname ctx ->
  finish ctx;
  check_lid ~place:"type_gen/tuple/name" tname;
  ctx.cur_type <- Some (Tuple_type (tname, []))

let tuple_arg1 = string_args1 & fun atype ctx ->
  match ctx.cur_type with
  | None | Some (Sum_type _ | Alias _ | Record_type _) ->
      failwith "tuple argument definition must be in tuple definition only"
  | Some (Tuple_type (tname, args)) ->
      let args = args @ [ atype ] in
      ctx.cur_type <- Some (Tuple_type (tname, args))
