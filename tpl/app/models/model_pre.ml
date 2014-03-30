(* [to_]delete must be generated only when id present *)

open Cd_All
open Cdt
open Strings.Latin1
open Schema_types
module Cg = Codegen
open Mlt

let sprintf = Printf.sprintf
let hashtbl_find_opt k h =
  try Some (Hashtbl.find h k) with Not_found -> None
let ( !! ) = Lazy.force


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

let col_ml_type c =
  let types = (Lazy.force schema).s_types in
  let t_ml_type tn = (Hashtbl.find types tn).ty_ml_name in
  t_ml_type c.cdc_type

(********************************************************************)

let model_desc = Memo.create ~eq:String.eq begin fun model_name ->
  let fname = "proj-build/app/models/" ^ model_name ^ ".desc.bin" in
  Opt_string_and_list_cdc_tm.from_file fname
end

let from_stored_model proj model_name =
  match model_desc model_name with
  | None -> failwith "Model have no description (%%table missing?)"
  | Some s -> proj s

let model_cols = from_stored_model snd
let model_table = from_stored_model fst

let self_model_name =
  Filename.chop_suffix (Filename.basename __mlt_filename) ".mlt"

let check_self_attr a =
  if List.exists
       (fun cdc -> cdc.cdc_name = a)
       (model_cols self_model_name)
  then ()
  else failwith "Attribute %S not found in the current schema" a

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
  "open Proj_common\nopen Forms_internal\nopen Models_internal\n\
   open Common_validations\n"

(********************************************************************)

let any_data_met = ref false

let check_no_data_met () =
  if !any_data_met
  then failwith "Validations must be defined before datasets"
  else ()

let validations = Queue.create ()

let vld_attrs mlt =
  begin match mlt with
  | `Str a -> [a]
  | `List l ->
      List.map
        (function
         | `Str a -> a
         | `List _ -> failwith
             "Validation attributes must be either string \
              or list of strings argument"
        )
        l
  end
  |> fun a ->
    begin
      if a = []
      then failwith "Validation must be applied to one or more attributes"
      else a
    end
  |> fun a ->
    begin
      List.iter check_self_attr a;
      a
    end


let out_validation_func attrs code =
  let basename = "__validate_" ^ String.concat "_" attrs in
  let name = gensym basename in
  out & directive_linedir ();
  out & Printf.sprintf
    "let %s %s __errors : unit =\n\
    \  try begin\n"
    name (String.concat " " attrs);
  out & directive_linedir ();
  out code;
  out "\n";
  out & Cg.line_directive "_model_codegenerator_" 0;
  out & Printf.sprintf
    "  end with e -> store_validation_error (%s) __errors e"
    (let ml_model = Cg.Lit.string self_model_name in
     match attrs with
     | [a] -> Cg.Sum.constr "Mvp_field" [ml_model; Cg.Lit.string a]
     | _ -> Cg.Sum.constr "Mvp_model" [ml_model]
    );
  out "\n;;\n";
  name


(* union-find *)
module Uf
 :
  sig
    type 'a t
    val create : 'a -> 'a t
    val union : 'a t -> 'a t -> unit
    val find : 'a t -> 'a t
    val get : 'a t -> 'a
  end
 =
  struct

    type 'a t =
      { v : 'a
      ; mutable up : 'a t
      ; mutable rank : int
      }

    let get uf = uf.v

    let create a =
      let rec uf = { v = a ; up = uf ; rank = 0 } in
      uf

    let rec find uf =
      if uf.up == uf
      then uf
      else begin
        uf.up <- find uf.up;
        uf.up
      end

    let union a b =
      let ra = find a
      and rb = find b in
      if ra == rb
      then ()
      else
        if ra.rank < rb.rank
        then
          ra.up <- rb
        else if ra.rank > rb.rank
        then
          rb.up <- ra
        else begin
          rb.up <- ra;
          ra.rank <- ra.rank + 1
        end

  end


(* these maps contain "validation groups", by key (some attribute of the group,
   it maps to list of attributes of this group), and by attribute (it maps to
   the key of this attribute's group).
 *)
let (vld_by_key, vld_by_attr) =
  let l = lazy begin
    let open Cadastr in
    let ti_uf_string = new tieq (Cdt.Simple "uf")
      ~eq:(fun a b -> Uf.get a = Uf.get b) () in
    let k2v = new Simp.map_rws_assoc ti_string
    and v2k = new Simp.map_rws_assoc ti_uf_string in
    let b = new Simp.bimap k2v v2k in
    let inj a = b#replace a (lazy (Uf.create a)) in
    Queue.iter
      (fun (attrs, name) ->
         Printf.printf "VLD: %S [%s]\n%!" name & String.concat " " attrs;
         let a1 = List.hd attrs in
         List.iter
           (fun a2 -> Uf.union (inj a1) (inj a2)
           )
           attrs
      )
      validations;
    let vld_by_key = new Simp.map_rws_assoc ti_string
    and vld_by_attr = new Simp.map_rws_assoc ti_string in
    b#iter
      (fun attr ufa ->
         let root = Uf.get (Uf.find ufa) in
         vld_by_key#replace root (attr :: vld_by_key#get_def root []);
         vld_by_attr#replace attr root
      );
    vld_by_key#iter
      (fun root attrs ->
         Printf.printf "VLD_GRP: %s = [%s]\n%!" root (String.concat " " attrs)
      );
    (vld_by_key, vld_by_attr)
  end
  in
    (lazy (fst !!l), lazy (snd !!l))


(********************************************************************)

let queue_to_list q =
  List.rev &
  Queue.fold (fun acc x -> x :: acc) [] q

let if_'__errors'_or_fail then_code =
  Cg.Expr.if_ "!__errors = []"
    (* then *)
    then_code
    (* else *)
    begin
      Cg.Expr.call_gen "Lwt.fail" & List.one &
        Cg.Sum.constr "Model_validation" ["!__errors"]
    end


let generate_insert_validations () =
  Cg.Expr.seq &
  List.map
    (fun (attrs, func_name) ->
       Cg.Expr.call func_name (attrs @ ["__errors"])
    ) &
  queue_to_list &
  validations

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
  Cg.Expr.let_in "__errors" "ref []" &
  Cg.Expr.seq
    [ generate_insert_validations ()
    ; if_'__errors'_or_fail &
        Cg.Expr.if_ "id = 0L"
          (gen false)
          (gen true)
    ]


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
           then Some (pg_col_to_param c "initial_id")
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


let instance_attr c ~upd_ind_opt ml_val =
  let n = c.cdc_name in
  begin
    if n = "id"
    then "val mutable initial_id = id\n"
    else ""
  end
  ^
  sprintf
     "val %s %s : %s = %s\n\
      method %s = %s\n"
     (if upd_ind_opt = None then "" else "mutable") n
       (col_ml_opt_type c) ml_val
     n n
  ^
  match upd_ind_opt with
  | Some ind ->
      Cg.method_ ("set_" ^ n) ["__x"] &
        Cg.Expr.seq
          [ n ^ " <- __x"
          ; Cg.Expr.call_gen "BitArray.set"
              [ "__modified_columns"
              ; Cg.Lit.int ind
              ; Cg.Lit.bool true
              ]
          ]
  | None ->
      ""

(* creates let-in environment with attributes and bound values
   got from forms or like.  [getval_prefix] rules how exactly form fields
   are processed -- it's a prefix of common function, suffixes are
   "_or_store_error" or "_opt_or_store_error" for mandatory/optional fields.
 *)
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


(* returns code packed in "OCaml sequence", which returns [unit]
   and appends validation errors to [__errors] (it's in environment).
 *)
let generate_update_validations vld_masks =
  Cg.Expr.seq &
  List.map
    (fun (mask_name, func_name, attrs) ->
       Cg.Expr.if_ ("BitArray.intersects __modified_columns " ^ mask_name)
         (* then *)
         (Cg.Expr.call func_name (attrs @ ["__errors"]))
         (* else *)
         "()"
    )
  vld_masks


(* todo: update должен быть returning *)

let generate_update ~vld_masks tname cols =
  Cg.Expr.if_ "BitArray.all_equal_to false __modified_columns"
    "return ()"
    begin
      Cg.Expr.let_in "__errors" "ref []" &
      Cg.Expr.let_in "()" (generate_update_validations vld_masks) &
      if_'__errors'_or_fail &
        begin
          Cg.Expr.let_in "__ug"
            (Cg.Expr.call "ug_create"
               [ Cg.Lit.string tname
               ; "initial_id"
               ]
            )
          begin
            Cg.Expr.seq
              ( List.mapi
                  (fun ind c ->
                     Cg.Expr.if_
                       (sprintf "BitArray.get __modified_columns %i" ind)
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
        end  (* when validation ok *)
    end (* all_equal_to false *)
  ^
  ">>= fun () ->\n\
   BitArray.fill_all false __modified_columns;\n\
   __record_status <- Rs_saved;\n\
   initial_id <- id;\n\
   return ()\n"


(* user can create/insert only when [cols] contains:
   - all mandatory attributes
   - all attributes that require validation
*)
let can_insert cols =
  let is_present attr = List.exists (fun c -> c.cdc_name = attr) cols in
  (!!vld_by_attr)#for_all (fun attr _grp -> is_present attr)
  &&
  let notnull_attrs = List.map_filter
    (fun c -> if c.cdc_nullable then None else Some c.cdc_name)
    (model_cols self_model_name) in
  List.for_all is_present notnull_attrs


(* "BitArray.make %i false" (List.length cols) *)
let instance_modifications_pre ~rec_st_ml ~upd_cols =
  if upd_cols = []
  then
    ""
  else begin
    sprintf
      "val mutable __record_status : record_status = (%s)\n" rec_st_ml
    ^
    sprintf
      "val __modified_columns : BitArray.t = __mod_cols\n"
  end


let instance_modifications_post ~vld_masks ~tname ~upd_cols ~cols =
  if upd_cols = []
  then ""
  else begin
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
        , generate_update ~vld_masks tname cols
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
      let col_args = List.map (fun c -> "?" ^ c.cdc_name) upd_cols in
      Cg.method_ "set_attrs" (col_args @ ["()"]) &
      Cg.Expr.seq &
        List.map
          (fun c ->
             let n = c.cdc_name in
             Cg.Expr.match_ n
             [ ("None", "()")
             ; ("Some x", sprintf "__self#set_%s x" n
               )
             ]
          )
          upd_cols
    end
    ^
    begin
      Cg.method_ "update_from_form" ["__m"] begin
        from_form_env ~getval_prefix:"update_from_form_field" upd_cols
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
                     (List.filter
                        (fun c -> c.cdc_name <> "id")
                        upd_cols
                     )
                  @ ["()"]
                  )
              ; "__self"
              ]
          ; "to_form"
          ; "(lazy __m)"
          ]
      end
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


let generate_internal_create ~ins ~vld_masks ~upd_cols cols =
  let tname = model_table self_model_name in
  let args_labelled = List.map (fun c -> "~" ^ c.cdc_name) cols in
  let internal_args =
    [ "~__record_status"
    ; sprintf "?(__mod_cols = BitArray.make %i false)"
        (List.length upd_cols)
    ]
  in
  begin
    Cg.Struc.func ~arg_per_line:true "__create_noopt"
      (internal_args @ args_labelled @ ["()"])
    begin
      begin
        if ins
        then ""
        else "\
          if __record_status = Rs_new\n\
          then invalid_arg \"can't create new record\"\n\
          else\n\
          "
      end
      ^
      "object (__self)\n"
      ^
      instance_modifications_pre
        ~rec_st_ml:"__record_status"
        ~upd_cols
      ^
      Cg.indent 2 begin
        String.concat "\n" begin
          List.map
            (fun c ->
               let upd_ind_opt =
                 match List.findi_opt ( ( == ) c ) upd_cols with
                 | None -> None
                 | Some (_found_col, ind) -> Some ind
               in
               instance_attr ~upd_ind_opt c c.cdc_name
            )
            cols
        end
      end
      ^
      instance_modifications_post ~tname ~vld_masks ~upd_cols ~cols
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
    Cg.Struc.func ~arg_per_line:true "__create" (args_opt @ ["()"]) &
      Cg.Expr.call ~newlines:true "__create_noopt" (args_labelled @ ["()"])
  end


(*
create для юзера не нужен, если всех столбцов с валидацией нет.
а для object_of_cols всегда нужен.
поэтому:
- __create[_noopt] будет всегда
- в случае, если создание возможно, будет let create = __create.
объект из object_of_cols должен уметь только такой save, который
за изменение-удаление отвечает.
*)
  

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
  Cg.Expr.call ~newlines:true "__create_noopt" create_args


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

(* for object created from form data, all attributes except [id]
   are marked as updated.
*)
let from_form ~cols ~upd_cols =
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
        Cg.Expr.call ~newlines:true "__create_noopt" &
          (
          "~__record_status:\
            (match __id_opt with None | Some 0L -> Rs_new | _ -> Rs_db)"
          ::
          begin
            let mask = BitArray.make (List.length upd_cols) true in
            let id_ind = snd &
              List.findi_exn (fun c -> c.cdc_name = "id") upd_cols in
            BitArray.set mask id_ind false;
            sprintf "~__mod_cols:(BitArray.of_repr_unsafe %S)"
              (BitArray.to_repr mask)
          end
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


(* prepares validations required for given [cols_inds].
   returns
     ( list of toplevel mask definitions to be glued before #save code
     , list of (mask_definition_name, vld_func_name, attrs)
     )
 *)
let generate_vld_masks cols_inds =
  let n = List.length cols_inds in
  let ind_of_attr attr =
    snd &
    List.get_single &
    List.filter (fun (c, _i) -> c.cdc_name = attr) cols_inds
  in
  Queue.fold
    (fun ((vld_masks_code, vld_masks) as acc) (attrs, func_name) ->
       let is_required =
         List.exists
           (fun attr ->
              List.exists
                (fun (col, _ind) ->
                   col.cdc_name = attr
                )
                cols_inds
           )
           attrs
       in
       if not is_required
       then acc
       else
         let mask = BitArray.make n false in
         List.iter
           (fun attr -> BitArray.set mask (ind_of_attr attr) true)
           attrs;
         let mask_name = gensym (func_name ^ "_mask") in
         let mask_code =
           Cg.Struc.expr ~ty:"BitArray.t" mask_name &
             Cg.Expr.call_gen "BitArray.of_repr_unsafe" & List.one &
               Cg.Lit.string (BitArray.to_repr mask)
         in
         ( (mask_code :: vld_masks_code)
         , ((mask_name, func_name, attrs) :: vld_masks)
         )
    )
    ([], [])
    validations


(* dataset has columns [cols] -- which of them are updatable wrt validation? *)
let updatable_cols cols =
  let attrs = List.map (fun c -> c.cdc_name) cols in
  List.filter
    (fun col ->
       let attr = col.cdc_name in
       (* attribute is updatable only when all attributes of its
          "validation group" is present in [cols]. *)
       match (!!vld_by_attr)#get_opt attr with
       | None ->
           (* attribute isn't used in validations => updatable *)
           true
       | Some grp ->
           let attrs_of_grp = (!!vld_by_key)#get_exn grp in
           List.for_all
             (fun agrp ->
                List.exists ( ( = ) agrp ) attrs
             )
             attrs_of_grp
    )
    cols


(* simple == for one table, one model, no special columns selection *)
let generate_fetcher_simple ~qname ~body ~single ~ctx =
  add_data qname ctx;
  let cols = model_cols self_model_name in
  let upd_cols = updatable_cols cols in
  (* in future, not every [cols] will be equal to [model_cols ..] *)
  let ins = can_insert cols in
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
  let (vld_masks_code, vld_masks) = generate_vld_masks cols_inds in
  out begin
  Cg.Struc.module_ qname_uid &
    [ to_form cols
    ]
    @
    vld_masks_code
    @
    [ generate_internal_create ~ins ~vld_masks ~upd_cols cols ]
    @ (if ins
       then [ "let create = __create"; from_form ~cols ~upd_cols ]
       else []
      )
    @
    [ Cg.Struc.func "load" (fetcher_args @ ["()"]) begin
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

let data1b qname ?single body = code & fun ctx ->
  let qname = expect_string "dataset name" qname
  and single = expect_string_opt "~single" single in
  let body = Cg.strip_line_directive body in
  let single =
    match single with
    | None -> false
    | Some str -> bool_of_string ~place:"~single" str
  in
  any_data_met := true;
  generate_fetcher_simple ~single ~qname ~body ~ctx

let validate2 mlt_vld_func mlt_attrs = code & fun _ctx ->
  check_no_data_met ();
  let vld_func = expect_string "validation function" mlt_vld_func in
  let attrs = vld_attrs mlt_attrs in
  assert (attrs <> []);
  List.iter
    (fun attr ->
       let attrs = [attr] in
       let func_name = out_validation_func
         attrs
         (Cg.Expr.call_gen vld_func attrs)
       in
       Queue.push (attrs, func_name) validations
    )
    attrs

let validate1b mlt_attrs body = code & fun _ctx ->
  let attrs = vld_attrs mlt_attrs in
  assert (attrs <> []);
  let name = out_validation_func attrs body in
  Queue.push (attrs, name) validations
(********************************************************************)

