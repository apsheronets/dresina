(* todo
разобраться, в каких случаях можно
1. создать инстанс (очевидно, когда insertable -- в выборке
   есть все обязательные поля)
2. менять конкретные поля (в cdc впилить таки cdc_upd, которое считать
   при разборе того, какие поля дают модели из запросов)
3. удалять инстанс ( = должно быть id), и поправить [to_]delete методы
   на этот случай.
*)


open Cd_All
open Strings.Latin1
open Schema_types
module Cg = Codegen
open Mlt

let sprintf = Printf.sprintf
let hashtbl_find_opt k h =
  try Some (Hashtbl.find h k) with Not_found -> None


let gather f = fun a ->
  match stage with
  | `Gather -> f a
  | `Code -> ()

let code f = fun a ->
  match stage with
  | `Code -> f a
  | `Gather -> ()

type context =
  { mutable cols : (string * column_def_checked list) option
  ; data_names : (string, unit) Hashtbl.t
  }

let schema = lazy
  (Schema_tm.from_file (Filename.concat "proj-build" schema_bin_fname))

let table1 = string_args1 & fun tname -> gather & fun ctx ->
  match ctx.cols with
  | Some _ -> failwith "duplicate 'table' directive"
  | None ->
      let schema = Lazy.force schema in
      let tab =
        try Hashtbl.find schema.s_tables tname
        with Not_found -> failwith
          "Can't find table %S in current schema" tname
      in
      ctx.cols <- Some (tname, tab.tab_cols)

(********************************************************************)

let model_desc = Memo.create ~eq:String.eq begin fun model_name ->
  let fname = "proj-build/app/models/" ^ model_name ^ ".desc.bin" in
  Opt_string_and_list_cdc_tm.from_file fname
end

let from_stored_model proj model_name =
  match model_desc model_name with
  | None -> failwith "Model have no description (%%table missed?)"
  | Some s -> proj s

let model_cols = from_stored_model snd
let model_table = from_stored_model fst

let self_model_name =
  Filename.chop_suffix (Filename.basename __mlt_filename) ".mlt"

let out_ctm = Memo.create ~eq:( = ) &
  let c = ref 0 in
  fun ctm ->
    let ident = sprintf "__ctm%i" (incr c; !c) in
    out & Cg.Struc.expr ident &
      "(Migrate_types.(" ^
      Migrate_types.ml_of_column_type_modifier ctm ^ "))\n";
    ident

let pg_to_string ty ctm ml_arg =
  sprintf "Schema_code.pg_%s_to_string %s %s"
    ty (out_ctm ctm) ml_arg

let pg_col_to_string col ml_arg =
  pg_to_string col.cdc_type col.cdc_type_mod ml_arg

let wrap_nullable nullable func ml_arg =
  if nullable
  then
    Cg.Expr.match_ ml_arg
      [ ("None", "Postgresql.null")
      ; ("Some x", func "x")
      ]
  else
    func ml_arg

let pg_col_to_param col ml_arg =
  wrap_nullable
    col.cdc_nullable
    (pg_to_string col.cdc_type col.cdc_type_mod)
    ml_arg

(* todo: types with near ctm to replace Ctm_none *)
let pg_sql_var_to_param nu ty ml_arg =
  wrap_nullable
    (match nu with `Nullable -> true | `Notnull -> false)
    (pg_to_string ty Migrate_types.Ctm_none)
    ml_arg

let pg_of_string col ml_arg =
  sprintf "Schema_code.pg_%s_of_string %s %s"
    col.cdc_type (out_ctm col.cdc_type_mod) ml_arg

let pg_of_data ~col ~colnum ~data ~row_ml =
  let str_ml = sprintf "(%s#getvalue %s %i)" data row_ml colnum in
  let v = pg_of_string col str_ml in
  if col.cdc_nullable
  then
    Cg.Expr.if_ (sprintf "%s#getisnull %s %i" data row_ml colnum)
      "None"
      (Cg.Sum.constr "Some" [v])
  else v

(********************************************************************)

let () = code out
  "open Proj_common\nopen Forms_internal\nopen Models_internal\n"

let generate_insert tname cols =
  let q n = "\"" ^ n ^ "\"" in
  let all_col_names_quoted = List.map (fun c -> q c.cdc_name) cols in
  let gen insert_id =
    let cols =
      if insert_id
      then cols
      else
        List.filter
          (fun c -> c.cdc_name <> "id")
          cols
    in
    let col_names_quoted = List.map (fun c -> q c.cdc_name) cols in
    Cg.Expr.let_in "sql" begin
      Cg.Lit.string & sprintf
      "insert into \"%s\" (%s) values (%s) returning %s"
      tname
      (String.concat "," col_names_quoted)
      (String.concat "," &
       List.mapi (fun i _n -> sprintf "$%i" (i + 1)) col_names_quoted
      )
      (String.concat "," all_col_names_quoted)
    end
    &
    Cg.Expr.let_in "params" begin
      Cg.Arr.constr ~newlines:true &
      List.map (fun c -> pg_col_to_param c c.cdc_name) cols
    end
    &
      "Database.pg_query_result ~params sql >>= fun data ->\n\
       ( __record_status <- Rs_saved;\n\
      \  __self#__fill_returning data;\n\
      \  return_unit\n\
       )"
  in
  Cg.Expr.if_ "id = 0L"
    (gen false)
    (gen true)


let generate_delete ~tname cols =
  Cg.Expr.let_in "sql" begin
    Cg.Lit.string & sprintf
    "delete from \"%s\" where id = $1"
    tname
  end
  &
  Cg.Expr.let_in "params" begin
    Cg.Arr.constr &
    let lst =
      List.map_filter
        (fun c ->
           let n = c.cdc_name in
           if n = "id"
           then Some (pg_col_to_param c n)
           else None
        )
        cols
    in
    ( assert (List.length lst = 1)
    ; lst
    )
  end
  &
    "Database.pg_command_ok ~params sql >>= fun () ->\n\
     ( __record_status <- Rs_deleted;\n\
    \  return_unit\n\
     )"


let col_ml_type c =
  let types = (Lazy.force schema).s_types in
  let t_ml_type tn = (Hashtbl.find types tn).ty_ml_name in
  let t = t_ml_type c.cdc_type in
  if c.cdc_nullable then "(" ^ t ^ ") option" else t

let instance_attr c ~ind ml_val =
  let n = c.cdc_name in
  sprintf
     "val mutable %s : %s = %s\n\
      method %s = %s\n"
     n (col_ml_type c) ml_val
     n n
  ^
  if true (* todo *)
  then
    Cg.method_ ("set_" ^ n) ["__x"] &
      Cg.Expr.seq
        [ n ^ " <- __x"
        ; Cg.Expr.call_gen "BitArray.set"
            [ "__modified_columns"
            ; Cg.Lit.int ind
            ; Cg.Lit.bool true
            ]
        ]
  else
    ""


let from_form_env ~getval_prefix cols =
  "let __errors = ref [] in\n\
   let __out_form = ref (lazy StrMap.empty) in\n"
  ^
  String.concat "" (
    List.map
      (fun c ->
         let n = c.cdc_name in
         sprintf "let __%s_opt =\n\
                 \  %s __errors __m __out_form\n\
                 \    (Schema_code.pg_%s_of_string %s) %S %S\nin\n"
           n
           (getval_prefix ^
            if c.cdc_nullable
            then "_opt_or_store_error"
            else "_or_store_error"
           )
           c.cdc_type
           (out_ctm c.cdc_type_mod)
           self_model_name
           n
      )
      cols
  )


(* todoc: про частичные/полные update -- универсального решения
   нет и не может быть.  в порядке убывания производительности
   надо предоставить: 1. частичный update, 2. полный update,
   3. select for update при загрузке данных из бд,
   4. блокировку таблиц.  Каждое для своих случаев.
   Чаще окажется так, что изменение только изменённых
   столбцов -- самое то.
   п.1,2,3 -- вполне можно через описание в модели.
 *)
(* todo: явно, изменение id не нужно.  надо вырезать set_id,
   в том числе из set_attrs.  и сделать более общо: некоторые поля
   должны быть неизменяемыми.  это в худшем случае описывать в
   запросе, а в лучшем случае -- смотреть на имена столбцов,
   которые запрашиваются (если явно "в модель такую-то -- столбцы
   такие-то из подзапроса t1"), и, если столбца нет в столбцах
   таблицы, то он неизменный.
   протащить это в cdc, там по умолчанию cdc_upd = true.
 *)
let generate_update tname cols =
  Cg.Expr.let_in "__mc" "__modified_columns" &
  Cg.Expr.if_ "BitArray.all_equal_to false __mc"
    "return ()"
    begin
      Cg.Expr.let_in "__ug"
        (Cg.Expr.call "ug_create"
           [ Cg.Lit.string tname
           ; "id"
           ]
        )
      begin
        Cg.Expr.seq
          ( List.mapi
              (fun ind c ->
                 Cg.Expr.if_
                   (sprintf "BitArray.get __mc %i" ind)
                   (Cg.Expr.call ~newlines:true "ug_add"
                      [ "__ug"
                      ; Cg.Lit.string c.cdc_name
                      ; pg_col_to_param c c.cdc_name
                      ]
                   )
                   ("()")
              )
              cols
          @ [ "ug_exec __ug" ]
          )
      end
    end
  ^
  ">>= fun () ->\n\
   BitArray.fill_all false __modified_columns;\n\
   __record_status <- Rs_saved;\n\
   return ()\n"


let instance_modifications_pre ~rec_st_ml cols =
  sprintf
    "val mutable __record_status : record_status = (%s)\n" rec_st_ml
  ^
  sprintf
    "val __modified_columns = BitArray.make %i false\n"
      (List.length cols)

let instance_modifications_post ~tname cols =
  begin
    Cg.method_ ~pvt:true "__fill_returning" ["data"] &
      Cg.Expr.seq &
      List.mapi
        (fun i col ->
           col.cdc_name
           ^ " <- "
           ^ pg_of_data ~col ~colnum:i ~data:"data"
               ~row_ml:(Cg.Lit.int 0)
        )
        cols
  end
  ^
  begin
    Cg.method_ "save_exn" ["()"] &
    Cg.Expr.match_ "__record_status"
    [ ( "Rs_new"
      , generate_insert tname cols
      )
    ; ( "Rs_db"
      , generate_update tname cols
      )
    ; ( "Rs_to_delete"
      , generate_delete ~tname cols
      )
    ; ( "Rs_saved | Rs_deleted"
      , "return_unit"
      )
    ]
  end
  ^
  begin
    Cg.method_ "save" ["()"] &
    sprintf "Forms_internal.catch_to_res_mvp %S __self#save_exn"
      self_model_name
  end
  ^
  begin
    Cg.method_ "to_delete" ["()"] &
    Cg.Expr.match_ "__record_status"
    [ ( "Rs_new"
      , "__record_status <- Rs_deleted"
      )
    ; ( "Rs_db | Rs_saved"
      , "__record_status <- Rs_to_delete"
      )
    ; ( "Rs_to_delete | Rs_deleted"
      , "()"
      )
    ]
  end
  ^
  begin
    Cg.method_ "delete" ["()"]
    "let () = __self#to_delete () in __self#save ()"
  end
  ^
  begin
    let col_args = List.map (fun c -> "?" ^ c.cdc_name) cols in
    Cg.method_ "set_attrs" (col_args @ ["()"]) &
    Cg.Expr.seq &
      List.map
        (fun c ->
           let n = c.cdc_name in
           Cg.Expr.match_ n
           [ ("None", "()")
           ; ("Some x", sprintf "__self#set_%s x" n
               (* потом, как с обновляемостью будет ясно, можно будет
                  в прямые присваивания переделать. *)
             )
           ]
        )
        cols
  end
  ^
  begin
    Cg.method_ "update_from_form" ["__m"] begin
      from_form_env ~getval_prefix:"update_from_form_field" cols
      ^
      Cg.Expr.call_gen ~newlines:true
        "Forms_internal.update_or_create_form"
        [ "!__errors"
        ; Cg.Lit.string self_model_name
        ; Cg.Expr.call ~newlines:true "lazy" & List.one &
          Cg.Expr.seq
            [ Cg.Expr.call_gen ~newlines:true "__self#set_attrs" &
                (List.map
                   (fun c ->
                      let n = c.cdc_name in
                      sprintf "?%s:(of_opt_exn %S __%s_opt)"
                        n
                        (sprintf "update %s.%s from form data"
                           self_model_name n)
                        n
                   )
                   cols
                @ ["()"]
                )
            ; "__self"
            ]
        ; "to_form"
        ; "(lazy __m)"
        ]
    end
  end


(* сгенерённое можно наследовать, чтобы переопределить нужные сериализации,
   для того и класс.
 *)

let to_form cols =
  let col_names = List.map (fun c -> c.cdc_name) cols in
  let names_patt = Cg.Tuple.constr ~newlines:true col_names in
  let body =
    Cg.Expr.let_in "__add_errors"
      (sprintf "Forms_internal.errors_of_exn_opt %S exn" self_model_name)
    &
    Cg.Expr.let_in names_patt
      (Cg.Expr.match_ "__arg"
         [ ( "`Instance obj"
           , Cg.Tuple.constr ~newlines:true begin
             List.map
               (fun c ->
                  let to_str_func = pg_col_to_string c in
                  let f = "obj#" ^ c.cdc_name in
                  if c.cdc_nullable
                  then Cg.Expr.match_ f
                    [ ("None", Cg.Lit.string "")
                    ; ("Some x", to_str_func "x")
                    ]
                  else to_str_func f
               )
               cols
             end
           )
         ; ( "`New"
           , Cg.Tuple.constr ~newlines:true &
             List.map (fun _ -> Cg.Lit.string "") col_names
           )
         ; ( "`Params m"
           , Cg.Tuple.constr ~newlines:true begin
             List.map
               (fun cn -> Cg.Expr.call "strmap_find_or"
                  [ Cg.Lit.string (self_model_name ^ "." ^ cn)
                  ; Cg.Lit.string ""
                  ; "m"
                  ]
               )
               col_names
             end
           )
         ]
      )
      (* expr: *)
      ( "object\n"
      ^ Cg.indent 2 (String.concat "\n"
          (sprintf
           "val model_errors_lazy = lazy (Forms_internal.errors_of_mvp \
             __add_errors __arg (Mvp_model %S))\n\
            method model_errors = Lazy.force model_errors_lazy"
           self_model_name
           ::
           sprintf
           "val all_errors_lazy = lazy (Forms_internal.all_model_errors \
             __add_errors __arg %S)\n\
            method all_errors = Lazy.force all_errors_lazy\n"
           self_model_name
           ::
           List.map
            (fun cn ->
               sprintf
                 "method %s =\n\
                  %s"
                 cn &
                 Cg.indent 2 &
                   sprintf
                     "object\n\
                     \  method v = %s\n\
                     \  method form_name = %S\n\
                     \  val errors = Forms_internal.errors_of_mvp __add_errors \
                           __arg \
                          (Mvp_field (%S, %S))\n\
                     \  method errors = errors\n\
                      end"
                     cn
                     (self_model_name ^ "." ^ cn)
                     self_model_name cn
            )
            col_names
          )
        )
      ^ "\nend"
      )
  in
  sprintf
    "class to_form ?exn __arg =\n%s\n"
    (Cg.indent 2 body)
  ^
    "let to_form ?exn arg = new to_form ?exn arg\n"


let generate_create cols =
  let tname = model_table self_model_name in
  let args_labelled = List.map (fun c -> "~" ^ c.cdc_name) cols in
  let internal_args = ["~__record_status"] in
  begin
(*
update тоже, кстати, returning.
обновлено или нет -- смотреть при save сравнением столбцов "из бд" и
значений из объекта.  Значения "из бд" можно прямо в строках хранить.
*)
    Cg.Struc.func ~arg_per_line:true "create_noopt"
      (internal_args @ args_labelled @ ["()"])
    begin
      (* все ошибки валидации закатывать в
         .. with e -> raise (Mvp_model "имямодели", e)
       *)
      "object (__self)\n"
      ^
      instance_modifications_pre
        ~rec_st_ml:"__record_status"
        cols
      ^
      Cg.indent 2 begin
        String.concat "\n" begin
          List.mapi
            (fun ind c -> instance_attr ~ind c c.cdc_name
            )
            cols
        end
      end
      ^
      instance_modifications_post ~tname cols
      ^
      begin
        Cg.method_ "to_form" ["?exn"; "()"] &
          "to_form ?exn (`Instance __self)"
      end
      ^ "\nend\n"
    end
  end
  ^
  let args_opt =
    List.map
      (fun c -> (if c.cdc_nullable then "?" else "~") ^ c.cdc_name
      )
      cols
  in
  begin
    Cg.Struc.func ~arg_per_line:true "create" (args_opt @ ["()"]) &
    Cg.Expr.call ~newlines:true "create_noopt" (args_labelled @ ["()"])
  end


(* assuming "data : Postgresql.result; row : int" in environment *)
let object_of_cols cols_inds =
  let create_args =
     List.map
       (fun (col, ind) ->
          sprintf "~%s:(%s)"
            col.cdc_name
            (pg_of_data ~col ~colnum:ind ~data:"data" ~row_ml:"row")
       )
       cols_inds
     @
     [ "~__record_status:Rs_db"
     ; "()"
     ]
  in
  Cg.Expr.call ~newlines:true "create_noopt" create_args


(* assuming "data : Postgresql.result" in environment,
   if single=false, creates "object 'mnames' : collection .. end",
   otherwise creates single instance, raising runtime error when
   there are not exactly one record returned.
 *)
let create_instances_simple ~mname ~single cols_inds =
  "let coll = new Collection.map2_id_ord in\n\
   let add = coll#add in\n\
   for row = 0 to data#ntuples - 1 do\n"
   ^
    Cg.indent 2 begin
      "add ~ord:row begin\n" ^
      Cg.indent 2 begin
        object_of_cols cols_inds
      end
      ^ "\nend"
    end
  ^
  "\ndone;\n" ^
  if single
  then
    "Proj_common.single_of_coll coll"
  else
    let mnames = Cg.Expr.lid (mname ^ "s")
      (* todo: %plural for manual 'mname'->'mnames' *)
    in
    sprintf "object method %s = coll end" (Cg.Expr.lid mnames)


let to_io code =
  "begin try return begin\n" ^ Cg.indent 2 code ^
  "\nend with e -> fail e end\n"

let add_data name ctx =
  if Hashtbl.mem ctx.data_names name
  then
    failwith "Generated module with name %S already exists" name
  else
    Hashtbl.add ctx.data_names name ()


let sql_tokenize sql =
  sql
  |> Lexing.from_string
  |> Sql_lexer.main

type var_ty = string (* type name *) * [`Nullable | `Notnull]

type sql_var =
  { sv_no : int
  ; mutable sv_ty_opt : var_ty option
  }

let dump_var_ty (t, n) =
  (match n with `Notnull -> "" | `Nullable -> "option ") ^ t

(* returns ("sql body with $1, $2", list of (identifier * type)) *)
let sql_vars lst =
  let h = Hashtbl.create 7 in
  let n = ref 0 in
  let add_var ident (opt_ty : var_ty option) =
    match hashtbl_find_opt ident h with
    | None -> Hashtbl.add h ident { sv_no = (incr n; !n); sv_ty_opt = opt_ty }
    | Some sv ->
        match sv.sv_ty_opt, opt_ty with
        | None, _ -> sv.sv_ty_opt <- opt_ty
        | _, None -> ()
        | Some sv_ty, Some ty ->
            if sv_ty = ty
            then ()
            else failwith "SQL variable %S has type %S, but got type %S later"
              ident (dump_var_ty sv_ty) (dump_var_ty ty)
  in
  let buf = Buffer.create 100 in
  let to_buf s = Buffer.add_string buf s in
  begin
    List.iter
      (fun tok ->
       begin match tok with
       | `Var (ident, opt_ty, _spc) -> add_var ident opt_ty
       | `Str _ -> ()
       end;
       begin match tok with
       | `Var (ident, _opt_ty, spc) ->
           to_buf & sprintf "$%i" (Hashtbl.find h ident).sv_no;
           to_buf spc
       | `Str (str, spc) ->
            to_buf str;
            to_buf spc
       end
      )
      lst;
    let vars = Hashtbl.fold
      (fun ident { sv_no = no ; sv_ty_opt = ty_opt } acc ->
         match ty_opt with
         | None -> failwith "SQL variable %S: type should be specified \
               (with \"@var : type\") at least once in the query"
             ident
         | Some ty ->
             (no, (ident, ty)) :: acc
      )
      h
      []
    in
    let vars_ordered =
      vars
      |> List.sort
           (fun (no1, _) (no2, _) -> Pervasives.compare no1 no2)
      |> List.map snd
    in
    (Buffer.contents buf, vars_ordered)
  end
        

let prepare_sql body =
  let tokens = sql_tokenize body in
  sql_vars tokens


(********************************************************************)

(* подумать, как тут сделать наследование (или что-то подобное),
   чтобы можно было из мапки заполнять поля объекта своими способами.
*)

let from_form cols =
  let self_id_k = self_model_name ^ ".id" in
  Cg.Struc.func "from_form" ["__m"] begin
    sprintf
    "let __m = match Proj_common.strmap_find_opt %S __m with\n\
    \  | None | Some \"\" -> StrMap.add %S \"0\" __m | _ -> __m in\n"
    self_id_k self_id_k
    ^
    from_form_env ~getval_prefix:"form_field" cols
    ^
    Cg.Expr.call_gen ~newlines:true
      "Forms_internal.create_inst_or_form"
      [ "!__errors"
      ; Cg.Lit.string self_model_name
      ; Cg.Expr.call ~newlines:true "lazy" & List.one &
        (* а вот и нет (в общем случае).
           create_noopt не знает про подмодели.
         *)
        Cg.Expr.call ~newlines:true "create_noopt" &
          ( "~__record_status:\
              (match __id_opt with None | Some 0L -> Rs_new | _ -> Rs_db)"
          ::
             (List.map
                (fun c ->
                   let n = c.cdc_name in
                   sprintf "~%s:(of_opt_exn %S __%s_opt)"
                     n
                     (sprintf "creating %s, field %s, from form data"
                        self_model_name n
                     )
                     n
                )
                cols
             @
               ["()"]
             )
          )
      ; "to_form"
      ; "(lazy __m)"
      ]
  end


(* simple == for one table, one model, no special columns selection *)
let generate_fetcher_simple ~qname ~body ~single ~ctx =
  add_data qname ctx;
  let cols = model_cols self_model_name in
  let cols_sql = String.concat ", " & List.map (fun c -> c.cdc_name) cols in
  let cols_inds = List.mapi (fun i c -> (c, i)) cols in
  let (body, vars) = body |> sql_tokenize |> sql_vars in
  let fetcher_args = List.map
    (fun (ident, _ty) -> "~" ^ ident)
    vars
  in
  let sql = "select " ^ cols_sql ^ " " ^ body in
  let ml_params = List.map
    (fun (ident, (ty, nu)) ->
       if Hashtbl.mem (Lazy.force schema).s_types ty
       then
         pg_sql_var_to_param nu ty ident
       else
         failwith "SQL variable %S: \
           can't find type %S in current schema" ident ty
    )
    vars
  in
  let qname_uid = Cg.uid (String.capitalize qname) in
  out begin
  Cg.Struc.module_ qname_uid
    [ to_form cols
    ; generate_create cols
    ; from_form cols
    ; Cg.Struc.func "load" (fetcher_args @ ["()"]) begin
        "Database.pg_query_result\n" ^
        Cg.Lit.string sql ^ "\n" ^
        "~params:\n" ^
        (Cg.indent 2 & Cg.Arr.constr ~newlines:true ml_params) ^
        ">>= fun data ->\n" ^
        (to_io &
         (* let tname = model_table self_model_name in *)
         create_instances_simple
           ~single ~mname:self_model_name cols_inds
        )
      end
    ]
  end

(********************************************************************)

let data1b qname ?single body = code & fun ctx ->
  let qname = expect_string "dataset name" qname
  and single = expect_string_opt "~single" single in
  let body = Cg.strip_line_directive body in
  let single =
    match single with
    | None -> false
    | Some str -> bool_of_string ~place:"~single" str
  in
  generate_fetcher_simple ~single ~qname ~body ~ctx
