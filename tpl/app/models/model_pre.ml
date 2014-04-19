(* [to_]delete must be generated only when id present *)

open Cd_All
open Cdt
open Strings.Latin1
open Schema_types
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

let col_ml_type c : string =
  let types = (Lazy.force schema).s_types in
  let t_ml_type tn = (Hashtbl.find types tn).ty_ml_name in
  t_ml_type c.cdc_type

let col_ml_opt_type c =
  let typ = Typ.prim & col_ml_type c in
  if c.cdc_nullable
  then Typ.param (Typ.prim "option") [typ]
  else typ

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

let col_of_self_attr a =
  List.find (fun cdc -> cdc.cdc_name = a) (model_cols self_model_name)

let q_opens = Queue.create ()
and q_ctms = Queue.create ()
and q_body = Queue.create ()

let qpush q x = Queue.push x q
let qpushlist q xs = List.iter (qpush q) xs

let emit_ctm = Memo.create ~eq:( = ) &
  let c = ref 0 in
  fun ctm ->
    let ident = sprintf "__ctm%i" (incr c; !c) in
    qpush q_ctms &
      Struc.expr ident &
        Expr.local_open ["Migrate_types"] &
        Migrate_types.expr_of_column_type_modifier ctm
    ;
    ident

let pg_to_string ty ctm expr =
  Expr.call_mod ["Schema_code"]
    (sprintf "pg_%s_to_string" ty)
    [ Expr.lid & emit_ctm ctm ; expr ]

let pg_col_to_string col ml_arg =
  pg_to_string col.cdc_type col.cdc_type_mod ml_arg

let wrap_nullable nullable (func : expr -> expr) (ml_arg : expr) =
  if nullable
  then
    Expr.match_ ml_arg
      [ (Patt.constr "None" [], Expr.lid_mod ["Postgresql"] "null")
      ; (Patt.(constr "Some" [lid "x"]), func & expr_lid "x")
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
  Expr.call_mod
    ["Schema_code"]
    (sprintf "pg_%s_of_string" col.cdc_type)
    [ Expr.lid & emit_ctm col.cdc_type_mod ; ml_arg ]

let pg_of_data ~col ~colnum ~data ~row_expr =
  let expr_val = Expr.meth data "getvalue" [row_expr; Expr.int colnum] in
  let v = pg_of_string col expr_val in
  if col.cdc_nullable
  then
    Expr.if_
      (Expr.meth data "getisnull" [row_expr; Expr.int colnum])
      (Expr.constr "None" [])
      (Expr.constr "Some" [v])
  else v

(********************************************************************)

let () = code begin fun () ->
  List.iter
    (fun m -> qpush q_opens & Struc.open_ [m])
    [ "Proj_common"; "Forms_internal"; "Models_internal"
    ; "Common_validations"
    ]
end ()

let output_from_form c meth =
  let ty = col_ml_type c in
  qpush q_body &
  Struc.expr
    ~typ:(Typ.arrow (List.map Typ.prim ["string"; ty]))
    ("from_form__" ^ c.cdc_name)
    (Expr.call_mod ["From_form"] (ty ^ "__" ^ meth)
       [Expr.lid & emit_ctm c.cdc_type_mod]
    )


let () = code begin fun () ->
  List.iter
    (fun c ->
       output_from_form c "default"
    )
    (model_cols self_model_name)
end
  ()

(********************************************************************)

let any_data_met = ref false

let check_no_data_met () =
  if !any_data_met
  then failwith "Validations must be defined before datasets"
  else ()

let validations : (string list * string) Queue.t = Queue.create ()

let attrs_of_mlt dir_name (mlt : mlt_val) =
  begin match mlt with
  | `Str a -> [a]
  | `List l ->
      List.map
        (function
         | `Str a -> a
         | `List _ -> failwith
             "%s attributes must be either string \
              or list of strings argument" dir_name
        )
        l
  end
  |> fun a ->
    begin
      if a = []
      then failwith "%s must be applied to one or more attributes" dir_name
      else a
    end
  |> fun a ->
    begin
      List.iter check_self_attr a;
      a
    end

let vld_attrs = attrs_of_mlt "Validation"
let from_form_attrs = attrs_of_mlt "From_form"

let out_validation_func attrs (code : expr) : string =
  let basename = "__validate_" ^ String.concat "_" attrs in
  let name = gensym basename in
  begin
    qpushlist q_body
      [ dir_struc_linedir ()
      ; Struc.func
          ~ret_typ:Typ.unit
          name
          (List.map Patt.lid & attrs @ ["__errors"]) &
          Expr.try_ begin
              (* Expr.linedir ld & *)
              code
            end
            [ ( Patt.lid "e"
              , Expr.call
                  "store_validation_error"
                  [ (let ml_model = Expr.string self_model_name in
                     match attrs with
                     | [a] -> Expr.constr
                         "Mvp_field"
                         [ml_model; Expr.string a]
                     | _ -> Expr.constr "Mvp_model" [ml_model]
                    )
                  ; expr_lid "__errors"
                  ; expr_lid "e"
                  ]
              )
            ]
      ]
  ; name
  end

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
  Expr.if_ (Expr.inj "!__errors = []")
    (* then *)
    then_code
    (* else *)
    begin
      Expr.call_mod ["Lwt"] "fail" & List.one &
        Expr.constr "Model_validation" [Expr.inj "!__errors"]
    end


let generate_insert_validations () =
  Expr.seq &
  List.map
    (fun (attrs, func_name) ->
       Expr.call
         func_name
         (List.map expr_lid & attrs @ ["__errors"])
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
    Expr.let_in (Patt.lid "sql") begin
      Expr.string & sprintf
        "insert into \"%s\" (%s) values (%s) returning %s"
        tname
        (String.concat "," col_names_quoted)
        (String.concat "," &
         List.mapi (fun i _n -> sprintf "$%i" (i + 1)) col_names_quoted
        )
        (String.concat "," all_col_names_quoted)
    end
    &
    Expr.let_in (Patt.lid "params") begin
      Expr.array &
      List.map (fun c -> pg_col_to_param c & expr_lid c.cdc_name) cols
    end
    &
      Expr.inj
      "Database.pg_query_result ~params sql >>= fun data ->\n\
       ( __record_status <- Rs_saved;\n\
      \  __self#__fill_returning data;\n\
      \  return_unit\n\
       )"
  in
  Expr.let_in (Patt.lid "__errors") (Expr.inj "ref []") &
  Expr.seq
    [ generate_insert_validations ()
    ; if_'__errors'_or_fail &
        Expr.if_ (Expr.inj "id = 0L")
          (gen false)
          (gen true)
    ]


let generate_delete ~tname cols =
  Expr.let_in (Patt.lid "sql") begin
    Expr.string & sprintf
    "delete from \"%s\" where id = $1"
    tname
  end
  &
  Expr.let_in (Patt.lid "params") begin
    Expr.array &
    let lst =
      List.map_filter
        (fun c ->
           let n = c.cdc_name in
           if n = "id"
           then Some (pg_col_to_param c & expr_lid "initial_id")
           else None
        )
        cols
    in
    ( assert (List.length lst = 1)
    ; lst
    )
  end
  &
    Expr.inj
    "Database.pg_command_ok ~params sql >>= fun () ->\n\
     ( __record_status <- Rs_deleted;\n\
    \  return_unit\n\
     )"


let instance_attr c ~upd_ind_opt ml_val : class_field list =
  let n = c.cdc_name in
  ( begin
      if n = "id"
      then [ Class.val_ ~mut:true "initial_id" (expr_lid "id") ]
      else []
    end
  )
  @
  [ Class.val_
      ~mut:(upd_ind_opt <> None)
      n
      ~typ:(col_ml_opt_type c)
      ml_val
  ; Class.method_ n [] (Expr.lid n)
  ]
  @
  ( match upd_ind_opt with
    | Some ind ->
        List.one &
        Class.method_ ("set_" ^ n) [Patt.lid "__x"] &
          Expr.seq
            [ Expr.inj (n ^ " <- __x")
            ; Expr.call_mod ["BitArray"] "set"
                [ expr_lid "__modified_columns"
                ; Expr.int ind
                ; Expr.bool true
                ]
            ]
    | None ->
        []
  )

(* creates let-in environment with attributes and bound values
   got from forms or like.  [getval_prefix] rules how exactly form fields
   are processed -- it's a prefix of common function, suffixes are
   "_or_store_error" or "_opt_or_store_error" for mandatory/optional fields.
 *)
let from_form_env ~getval_prefix cols : let_ins =
  List.reduce_left identity Let_in.append &
  [
    Let_in.make (Patt.lid "__errors") & Expr.inj "ref []"
  ;
    Let_in.make (Patt.lid "__out_form") & Expr.inj "ref (lazy StrMap.empty)"
  ]
  @
    List.map
      (fun c ->
         let n = c.cdc_name in
         Let_in.make (Patt.lid & "__" ^ n ^ "_opt") &
           Expr.call
             (getval_prefix ^
              if c.cdc_nullable
              then "_opt_or_store_error"
              else "_or_store_error"
            )
            [ expr_lid "__errors"
            ; expr_lid "__m"
            ; expr_lid "__out_form"
            ; expr_lid & "from_form__" ^ n
            ; Expr.string self_model_name
            ; Expr.string n
            ]
      )
      cols


(* returns code packed in "OCaml sequence", which returns [unit]
   and appends validation errors to [__errors] (it's in environment).
 *)
let generate_update_validations vld_masks =
  Expr.seq &
  List.map
    (fun (mask_name, func_name, attrs) ->
       Expr.if_
         (Expr.call_mod ["BitArray"] "intersects"
            [ expr_lid "__modified_columns"
            ; Expr.lid mask_name
            ]
         )
         (* then *)
         (Expr.call
            func_name
            (List.map expr_lid & attrs @ ["__errors"])
         )
         (* else *)
         Expr.unit
    )
  vld_masks


(* todo: update должен быть returning *)

let generate_update ~vld_masks tname cols =
  let __ug = Expr.lid "__ug" in
  Expr.if_ (Expr.inj "BitArray.all_equal_to false __modified_columns")
    (* then *)
    (Expr.inj "return ()")
    (* else *)
    begin
      Expr.let_in (Patt.lid "__errors") (Expr.inj "ref []") &
      Expr.let_in Patt.unit (generate_update_validations vld_masks) &
      if_'__errors'_or_fail &
        begin
          Expr.let_in (Patt.lid "__ug")
            (Expr.call "ug_create"
               [ Expr.string tname
               ; expr_lid "initial_id"
               ]
            )
          begin
            Expr.seq
              ( List.mapi
                  (fun ind c ->
                     Expr.if_
                       (Expr.call_mod
                          ["BitArray"]
                          "get"
                          [ expr_lid "__modified_columns"
                          ; Expr.int ind
                          ]
                       )
                       (Expr.call "ug_add"
                          [ __ug
                          ; Expr.string c.cdc_name
                          ; pg_col_to_param c & expr_lid c.cdc_name
                          ]
                       )
                       (Expr.unit)
                  )
                  cols
              @ [ Expr.call ("ug_exec") [ __ug ] ]
              )
          end
        end  (* when validation ok *)
    end (* all_equal_to false *)
  /* Expr.infix ">>=" */
  Expr.inj
  "fun () ->\n\
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

let bitarray_t = Typ.prim ~mod_path:["BitArray"] "t"

(* "BitArray.make %i false" (List.length cols) *)
let instance_modifications_pre ~rec_st_expr ~upd_cols : class_field list =
  if upd_cols = []
  then
    []
  else
    [ Class.val_ ~mut:true "__record_status"
        ~typ:(Typ.prim "record_status")
        rec_st_expr
    ; Class.val_ "__modified_columns"
        ~typ:bitarray_t &
        expr_lid "__mod_cols"
    ]

let self_meth m ?lab args =
  let em = Expr.meth (expr_lid "__self") m [] in
  Expr.app em ?lab args

let instance_modifications_post ~vld_masks ~tname ~upd_cols ~cols
 :
  class_field list
 =
  if upd_cols = []
  then
    []
  else
    [
      Class.method_ ~pvt:true "__fill_returning" [Patt.lid "data"] &
        Expr.seq &
        List.mapi
          (fun i col ->
             expr_lid col.cdc_name
             /* Expr.infix "<-" */
             pg_of_data ~col ~colnum:i ~data:(expr_lid "data")
               ~row_expr:(Expr.int 0)
          )
          cols
    ;
      Class.method_ "save_exn" [Patt.unit] &
      Expr.match_ (expr_lid "__record_status")
      [ ( Patt.constr "Rs_new" []
        , generate_insert tname cols  (* todo: validations here? *)
        )
      ; ( Patt.constr "Rs_db" []
        , generate_update ~vld_masks tname cols
        )
      ; ( Patt.constr "Rs_to_delete" []
        , generate_delete ~tname cols
        )
      ; ( Patt.(alt [constr "Rs_saved" []; constr "Rs_deleted" []])
        , expr_lid "return_unit"
        )
      ]
    ;
    begin
      Class.method_ "save" [Patt.unit] &
      Expr.call_mod ["Forms_internal"] "catch_to_res_mvp"
        [ Expr.string self_model_name
        ; self_meth "save_exn" []
        ]
    end
    ;
    begin
      Class.method_ "to_delete" [Patt.unit] &
      let rs = expr_lid "__record_status" in
      let set_rs x = rs /* Expr.infix "<-" */ x in
      Expr.match_ rs
      [ ( Patt.constr "Rs_new" []
        , set_rs & Expr.constr "Rs_deleted" []
        )
      ; ( Patt.(alt [constr "Rs_db" [] ; constr "Rs_saved" []])
        , set_rs & Expr.constr "Rs_to_delete" []
        )
      ; ( Patt.(alt [constr "Rs_to_delete" [] ; constr "Rs_deleted" []])
        , Expr.unit
        )
      ]
    end
    ;
    begin
      Class.method_ "delete" [Patt.unit] &
      Expr.seq
        [ self_meth "to_delete" [Expr.unit]
        ; self_meth "save" [Expr.unit]
        ]
    end
    ;
    begin
      let col_args =
        List.map
          (fun c -> Patt.opt c.cdc_name)
          upd_cols
      in
      Class.method_ "set_attrs" (col_args @ [Patt.unit]) &
      Expr.seq &
        List.map
          (fun c ->
             let n = c.cdc_name in
             Expr.match_ (expr_lid n)
             [ ( Patt.constr "None" [], Expr.unit )
             ; ( Patt.(constr "Some" [lid "x"])
               , self_meth ("set_" ^ n) [expr_lid "x"]
               )
             ]
          )
          upd_cols
    end
    ;
      Class.method_ "update_from_form" [Patt.lid "__m"] begin
        from_form_env ~getval_prefix:"update_from_form_field" upd_cols
        /* Expr.prepend_let_ins */
        Expr.call_mod
          ["Forms_internal"]
          "update_or_create_form"
          [ Expr.inj "!__errors"
          ; Expr.string self_model_name
          ; Expr.lazy_ &
            Expr.seq
              [ self_meth "set_attrs"
                  ~lab:
                  (List.map_filter
                     (fun c ->
                        let n = c.cdc_name in
                        if n = "id" then None else
                        some &
                        Arg.opt n &
                        Expr.call "of_opt_exn"
                          [ Expr.string 
                              (sprintf "update %s.%s from form data"
                                 self_model_name n)
                          ; expr_lid & sprintf "__%s_opt" n
                          ]
                     )
                     upd_cols
                  )
                  [Expr.unit]
              ; expr_lid "__self"
              ]
          ; expr_lid "to_form"
          ; Expr.lazy_ & expr_lid "__m"
          ]
        end
    ]

let lazy_force e = Expr.call_mod ["Lazy"] "force" [e]

let lazy_val_meth n e =
  [ Class.val_ (n ^ "_lazy") &
      Expr.lazy_ e
  ; Class.method_ n [] &
      lazy_force (expr_lid & n ^ "_lazy")
  ]

(* сгенерённое можно наследовать, чтобы переопределить нужные сериализации,
   для того и класс.
 *)

let to_form cols : struc_item list =
  let col_names = List.map (fun c -> c.cdc_name) cols in
  let names_patt = Patt.tuple & List.map Patt.lid col_names in
  let __add_errors = expr_lid "__add_errors"
  and __arg = expr_lid "__arg" in
  let pre_body : let_ins =
    Let_in.make (Patt.lid "__add_errors")
      (Expr.call_mod ["Forms_internal"] "errors_of_exn_opt"
         [ Expr.string self_model_name
         ; expr_lid "exn"
         ]
      )
    /* Let_in.append */
    Let_in.make names_patt
      (Expr.match_ __arg
         [ ( Patt.(poly "`Instance" [lid "obj"])
           , Expr.tuple begin
             List.map
               (fun c ->
                  let to_str_func = pg_col_to_string c in
                  let f = Expr.meth (expr_lid "obj") c.cdc_name [] in
                  if c.cdc_nullable
                  then Expr.match_ f
                    [ (Patt.constr "None" [], Expr.string "")
                    ; ( Patt.(constr "Some" [lid "x"])
                      , to_str_func & expr_lid "x"
                      )
                    ]
                  else to_str_func f
               )
               cols
             end
           )
         ; ( Patt.poly "`New" []
           , Expr.tuple &
             List.map (fun _ -> Expr.string "") col_names
           )
         ; ( Patt.(poly "`Params" [lid "m"])
           , Expr.tuple begin
             List.map
               (fun cn -> Expr.call "strmap_find_or"
                  [ Expr.string (self_model_name ^ "." ^ cn)
                  ; Expr.string ""
                  ; expr_lid "m"
                  ]
               )
               col_names
             end
           )
         ]
      )
  in
  let body : class_field list =
    (lazy_val_meth "model_errors" &
       Expr.call_mod ["Forms_internal"]
         "errors_of_mvp"
         [ __add_errors
         ; expr_lid "__arg"
         ; Expr.constr "Mvp_model" [Expr.string self_model_name]
         ]
    )
    @
    (lazy_val_meth "all_errors" &
       Expr.call_mod ["Forms_internal"] "all_model_errors"
         [ __add_errors
         ; __arg
         ; Expr.string self_model_name
         ]
    )
    @
    (List.map
       (fun cn ->
          Class.method_ cn [] &
          Expr.object_
            [ Class.method_ "v" [] (expr_lid cn)
            ; Class.method_ "form_name" [] &
                Expr.string & self_model_name ^ "." ^ cn
            ; Class.val_ "errors" &
                Expr.call_mod ["Forms_internal"] "errors_of_mvp"
                  [ __add_errors
                  ; __arg
                  ; Expr.constr "Mvp_field"
                      [ Expr.string self_model_name
                      ; Expr.string cn
                      ]
                  ]
            ; Class.method_ "errors" [] & expr_lid "errors"
            ]
       )
       col_names
    )
  in
  [ Struc.class_ "to_form" Patt.([opt "exn" ; lid "__arg"])
      ~pre:pre_body
      body
  ; Struc.func "to_form" Patt.([opt "exn"; lid "arg"]) &
      Expr.new_ "to_form"
        ~lab:[Arg.opt_self "exn"]
        [expr_lid "arg"]
  ]


let generate_internal_create ~ins ~vld_masks ~upd_cols cols =
  let tname = model_table self_model_name in
  let args_labelled_patt = List.map (fun c -> Patt.lab c.cdc_name) cols
  and args_labelled = List.map (fun c -> Arg.lab_self c.cdc_name) cols in
  let internal_args =
    [ Patt.lab "__record_status"
    ; Patt.opt "__mod_cols" ~def:(
        Expr.call_mod ["BitArray"] "make"
          [ Expr.int & List.length upd_cols
          ; Expr.bool false
          ]
      )
    ]
  in
  [ Struc.func "__create_noopt"
      (internal_args @ args_labelled_patt @ [Patt.unit]) &
    Expr.prepend_let_ins
    (if ins
     then Let_in.empty
     else Let_in.make Patt.unit &
       Expr.if_ (Expr.inj "__record_status = Rs_new")
         (Expr.call "invalid_arg"
            [Expr.string "can't create new record"]
         )
         Expr.unit
    )
    (Expr.object_ ~self:"__self" &
       instance_modifications_pre
         ~rec_st_expr:(expr_lid "__record_status")
         ~upd_cols
       @
       List.flatten (List.map
         (fun c ->
            let upd_ind_opt =
              match List.findi_opt ( ( == ) c ) upd_cols with
              | None -> None
              | Some (_found_col, ind) -> Some ind
            in
            instance_attr ~upd_ind_opt c & expr_lid c.cdc_name
         )
         cols
       )
      @
      instance_modifications_post ~tname ~vld_masks ~upd_cols ~cols
      @
      [
      Class.method_ "to_form" [Patt.opt "exn"; Patt.unit] &
        Expr.call "to_form"
          ~lab:[Arg.opt_self "exn"]
          [Expr.inj "`Instance __self"]  (* todo Expr.poly *)
      ]
    )
  ;
    let args_opt =
      List.map
        (fun c ->
           let n = c.cdc_name in
           if c.cdc_nullable
           then Patt.opt n
           else Patt.lab n
        )
        cols
    in
    begin
      Struc.func "__create" (args_opt @ [Patt.unit]) &
        Expr.call "__create_noopt" ~lab:args_labelled [Expr.unit]
    end
  ]


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
  let create_args_lab =
     List.map
       (fun (col, ind) ->
          Arg.lab col.cdc_name
            (pg_of_data ~col ~colnum:ind ~data:(expr_lid "data")
               ~row_expr:(expr_lid "row"))
       )
       cols_inds
     @
     [ Arg.lab "__record_status" (Expr.constr "Rs_db" [])
     ]
  and create_args = [ Expr.unit ]
  in
  Expr.call "__create_noopt" ~lab:create_args_lab create_args


(* assuming "data : Postgresql.result" in environment,
   if single=false, creates "object 'mnames' : collection .. end",
   otherwise creates single instance, raising runtime error when
   there are not exactly one record returned.
 *)
let create_instances_simple ~mname ~single cols_inds =
  Expr.let_in (Patt.lid "coll")
    (Expr.new_ ~mod_path:["Collection"] "map2_id_ord" []) &
  Expr.let_in (Patt.lid "add")
    (Expr.meth (expr_lid "coll") "add" []) &
  Expr.seq
    [
      Expr.for_ "row" (Expr.int 0) (Expr.inj "data#ntuples - 1") &
        Expr.call "add" ~lab:[Arg.lab "ord" & expr_lid "row"]
          [ object_of_cols cols_inds ]
    ; if single
      then
        Expr.call_mod ["Proj_common"] "single_of_coll"
          [expr_lid "coll"]
      else
        let mnames = mname ^ "s"
          (* todo: %plural for manual 'mname'->'mnames' *)
        in
        Expr.object_ [ Class.method_ mnames [] & expr_lid "coll" ]
    ]

let to_io expr =
  Expr.try_ (Expr.call_mod ["IO"] "return" [expr])
    [ (Patt.lid "e", Expr.call "fail" [expr_lid "e"]) ]

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
  let self_id_str_e = Expr.string & self_model_name ^ ".id" in
  let __m = expr_lid "__m" in
  Struc.func "from_form" [Patt.lid "__m"] begin
    ( (Let_in.make (Patt.lid "__m") &
         let find_res_expr = Expr.call_mod ["Proj_common"]
           "strmap_find_opt"
           [ self_id_str_e ; Expr.lid "__m" ]
         in
         Expr.match_ find_res_expr
           [ ( Patt.(alt [constr "None" []; constr "Some" [string ""]])
             , Expr.call_mod ["StrMap"] "add"
                 [ self_id_str_e ; Expr.string "0" ; __m ]
             )
           ; ( Patt.any
             , __m
             )
           ]
      )
      /* Let_in.append */
      from_form_env ~getval_prefix:"form_field" cols
    )
    /* Expr.prepend_let_ins */
    Expr.call_mod ["Forms_internal"] "create_inst_or_form"
      [ Expr.inj "!__errors"
      ; Expr.string self_model_name
      ; Expr.lazy_ &
        (* а вот и нет (в общем случае).
           create_noopt не знает про подмодели.
         *)
        Expr.call "__create_noopt"
          ~lab:
          (
          begin
            Arg.lab "__record_status" &
              Expr.match_ (expr_lid "__id_opt")
              [ ( Patt.(alt [constr "None" [] ; constr "Some" [int64 0L]])
                , Expr.constr "Rs_new" []
                )
              ; ( Patt.any, Expr.constr "Rs_db" [] )
              ]
          end
          ::
          begin
            let mask = BitArray.make (List.length upd_cols) true in
            let id_ind = snd &
              List.findi_exn (fun c -> c.cdc_name = "id") upd_cols in
            BitArray.set mask id_ind false;
            Arg.lab "__mod_cols" &
              Expr.call_mod ["BitArray"] "of_repr_unsafe"
                [ Expr.string & BitArray.to_repr mask ]
          end
          ::
             (List.map
                (fun c ->
                   let n = c.cdc_name in
                   Arg.lab n &
                     Expr.call "of_opt_exn"
                       [ Expr.string &
                           sprintf "creating %s, field %s, from form data"
                             self_model_name n
                       ; expr_lid &
                           sprintf "__%s_opt" n
                       ]
                )
                cols
            )
          )
          [Expr.unit]
      ; expr_lid "to_form"
      ; Expr.lazy_ & expr_lid "__m"
      ]
  end


(* prepares validations required for given [cols_inds].
   returns
     ( list of toplevel mask definitions to be glued before #save code
     , list of (mask_definition_name, vld_func_name, attrs)
     )
 *)
let generate_vld_masks cols_inds
 :
  ( _
  * (lid * string * string list) list
  )
 =
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
           Struc.expr ~typ:bitarray_t mask_name &
             Expr.call_mod ["BitArray"] "of_repr_unsafe" &
               [ Expr.string (BitArray.to_repr mask) ]
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
  let (_fname, _lineno, bodycode) = body in
  let (bodycode, vars) = bodycode |> sql_tokenize |> sql_vars in
  let fetcher_args = List.map
    (fun (ident, _ty) -> Patt.lab ident)
    vars
  in
  let sql = "select " ^ cols_sql ^ " " ^ bodycode in
  let ml_params = List.map
    (fun (ident, (ty, nu)) ->
       if Hashtbl.mem (Lazy.force schema).s_types ty
       then
         pg_sql_var_to_param nu ty (expr_lid ident)
       else
         failwith "SQL variable %S: \
           can't find type %S in current schema" ident ty
    )
    vars
  in
  let qname_uid = String.capitalize qname in
  let (vld_masks_code, vld_masks) = generate_vld_masks cols_inds in
  qpush q_body &
  Struc.module_ qname_uid &
    to_form cols
    @
    vld_masks_code
    @
    generate_internal_create ~ins ~vld_masks ~upd_cols cols
    @ (if ins
       then [ Struc.expr "create" (expr_lid "__create")
            ; from_form ~cols ~upd_cols
            ]
       else []
      )
    @
    [ Struc.func "load" (fetcher_args @ [Patt.unit]) begin
        (Expr.call_mod ["Database"] "pg_query_result"
           [ Expr.string sql ]
           ~lab:
           [ Arg.lab "params" & Expr.array ml_params ]
        )
        /* Expr.infix ">>=" */
        Expr.fun_ [Patt.lid "data"]
        (to_io &
         (* let tname = model_table self_model_name in *)
         create_instances_simple
           ~single ~mname:self_model_name cols_inds
        )
      end
    ]


let output_from_form_attrs attrs meth =
  List.iter
    (fun attr ->
       let col = col_of_self_attr attr in
       output_from_form col meth
    )
    attrs

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
       let () = Printf.eprintf "DBG FU: %s\n%!" vld_func in
       let func_name = out_validation_func
         attrs
         (Expr.app
            (Expr.of_body & make_body vld_func) &
            List.map Expr.lid attrs
         )
       in
       Queue.push (attrs, func_name) validations
    )
    attrs

let validate1b mlt_attrs body = code & fun _ctx ->
  let attrs = vld_attrs mlt_attrs in
  assert (attrs <> []);
  let name = out_validation_func attrs (Expr.of_body body) in
  Queue.push (attrs, name) validations

let from_form2 mlt_attrs mlt_meth = code & fun _ctx ->
  let attrs = from_form_attrs mlt_attrs in
  assert (attrs <> []);
  let meth = expect_string "'from_form method'" mlt_meth in
  output_from_form_attrs attrs meth

(********************************************************************)

