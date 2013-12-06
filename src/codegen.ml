open Cd_All
open Strings.Latin1
open Printf

let failwith fmt = Printf.ksprintf failwith fmt
let invalid_arg fmt = Printf.ksprintf invalid_arg fmt
let dbg fmt = Printf.ksprintf (fun s -> Printf.eprintf "DBG: %s\n%!" s) fmt

(*
let ( & ) f x = f x
let ( @> ) f g x = g (f x)
let 
*)


let check_failed ~place txt = invalid_arg "%s: %s" place txt

let ident_ok n =
  let rec loop i =
    if i = String.length n
    then true
    else
      let c = n.[i] in
      if    (c >= '0' && c <= '9')
         || (c >= 'A' && c <= 'Z')
         || (c >= 'a' && c <= 'z')
         || c = '_'
         || c = '\''
      then
        loop (i + 1)
      else
        false
  in
    loop 1

let check_uid ~place n =
  if n = ""
  then
    check_failed ~place "uppercase identifier can't be empty"
  else
    let c = n.[0] in
    if c >= 'A' && c <= 'Z'
    then
      if ident_ok n
      then ()
      else check_failed ~place "uppercase identifier: bad characters"
    else check_failed ~place "uppercase identifier must begin with 'A'..'Z'"

let check_lid ~place n =
  if n = ""
  then
    check_failed ~place "lowercase identifier can't be empty"
  else
    let c = n.[0] in
    if c >= 'a' && c <= 'z' || c = '_'
    then
      if ident_ok n
      then ()
      else check_failed ~place "lowercase identifier: bad characters"
    else
      check_failed ~place
        "lowercase identifier must begin with 'a'..'z' or '_'"

module Lit
 =
  struct
    let string s = "\"" ^ String.escaped s ^ "\""
    let int = string_of_int
  end

(* for ml_of_* : *)
let ml_of_string = Lit.string
let ml_of_int = Lit.int

let codegen_cur_indent = ref 0
let do_indent ml =
  let ind = !codegen_cur_indent in
  if ind = 0
  then ml
  else
    let pre = String.make ind '\x20' in
    ml |>
    String.split_exact ( ( = ) '\n' ) |>
    List.map
      (fun line ->
         if line <> "" && line.[0] = '#'  (* line directive *)
         then line
         else pre ^ line
      ) |>
    String.concat "\n"

let indent add ml =
  assert (add > 0);
  let old_ind = !codegen_cur_indent in
  let finally () = codegen_cur_indent := old_ind in
  try
    codegen_cur_indent := old_ind + add;
    let ml = do_indent ml in
    finally ();
    ml
  with
  | e -> (finally (); raise e)

module Tuple
 =
  struct
    let check_arity ~fn args =
      if List.length args > 1
      then ()
      else
        invalid_arg
          "Codegen.Tuple.%s: must have more than one type argument" fn

    let constr args =
      check_arity args ~fn:"constr";
      "(" ^ String.concat ", " args ^ ")"

    let typedef type_name args =
      check_lid ~place:"Codegen.Tuple.typedef" type_name;
      check_arity ~fn:"typedef" args;
      sprintf "type %s = %s\n;;\n" type_name &
        String.concat " * " args
  end

module Sum
 =
  struct
    let constr constr_name args =
      check_uid ~place:"Codegen.Sum.constr" constr_name;
      sprintf "%s%s"
        constr_name
        (match args with
         | [] -> ""
         | a :: [] -> " " ^ a
         | _ -> " " ^ Tuple.constr args
        )

    let typedef_constr (cname, ctypes) =
      check_uid ~place:"Codegen.Sum.typedef_constr" cname;
      if ctypes = []
      then cname
      else
        sprintf "%s of %s" cname &
          String.concat " * " ctypes

    let typedef type_name constrs =
      check_lid ~place:"Codegen.Sum.typedef" type_name;
      sprintf "type %s =\n%s;;\n" type_name &
        String.concat "" &
        List.map (typedef_constr @> sprintf "| %s\n") constrs

  end

let string_index_opt s c =
  try Some (String.index s c) with Not_found -> None

module Typ
 =
  struct
    let prim n =
      check_lid ~place:"Codegen.Typ.prim" n;
      n

    let arrow lst =
      if List.length lst < 2
      then invalid_arg "Codegen.Typ.arrow: must have >= 2 type arguments"
      else
        sprintf "(%s)" &
          String.concat " -> " lst

    let param ty args =
      check_lid ~place:"Codegen.Typ.param" ty;
      match args with
      | [] -> invalid_arg "Codegen.Typ.param: not a parametrized type"
      | h :: [] -> sprintf "%s %s" h ty
      | _ ->
         let args_code = String.concat ", " args in
         sprintf "(%s) %s" args_code ty
  end


let format_mids ~when_0 ~when_1 ~first ~mid ~last lst =
  match lst with
  | [] -> when_0 ()
  | h :: [] -> when_1 h
  | h :: (m :: t) ->
      first h;
      let rec inner h t =
        match t with
        | h' :: t' -> mid h; inner h' t'
        | [] -> last h
      in
        inner m t


module Expr
 =
  struct
    let list lst =
      let buf = Buffer.create 100 in
      begin format_mids
        ~when_0:(fun () -> Buffer.add_string buf "[]")
        ~when_1:(bprintf buf "[ %s ]")
        ~first: (bprintf buf "[ %s;\n")
        ~mid:   (bprintf buf "  %s;\n")
        ~last:  (bprintf buf "  %s\n]\n")
        lst
      end;
      Buffer.contents buf

    (* can call non-qualified functions only; without "Module." parts *)
    let call func_name args =
      check_lid ~place:"Codegen.Expr.call" func_name;
      if args = []
      then invalid_arg
        "Codegen.Expr.call: can't call function without arguments"
      else
      sprintf "(%s %s)" func_name &
        String.concat " " &
        List.map (sprintf "(%s)") args

    let lid n =
      check_lid ~place:"Codegen.Expr.lid" n;
      n

    let modqual mod_name mod_comp =
      check_uid mod_name ~place:"Codegen.Expr.modqual";
      mod_name ^ "." ^ mod_comp

    (* [match_ "lst" [ ("[]", "None") ; ("h :: t", "Some (h, t)") ]] *)
    let match_ expr branches =
      sprintf
        "begin match %s with\n%s\nend"
        expr
        (String.concat "\n" &
         List.map
           (fun (patt, expr) ->
              sprintf "| %s ->\n%s" patt (indent 4 expr)
           )
           branches
        )

    let let_in ?(oneline=false) patt binding expr =
      if oneline
      then
        sprintf "let %s = %s in\n%s"
          patt binding expr
      else
        sprintf "let %s =\n%s\nin\n%s"
          patt (indent 2 binding) expr

    let seq expr_list =
      match expr_list with
      | [] -> invalid_arg "Codegen.Expr.seq: empty sequence"
      | e :: [] -> e
      | _ ->
          sprintf "begin\n%s\nend"
            (indent 2 & String.concat ";\n" expr_list)

    let for_ var_ from_ to_ body_seq =
      sprintf "for %s = %s to %s do\n%s\ndone"
        (lid var_) from_ to_ (indent 2 & String.concat ";\n" body_seq)
  end

module Struc
 =
  struct
    let expr ?ty name body =
      if name = "()"
      then ()
      else check_lid ~place:"Codegen.Struc.expr" name;
      let opt_ty =
        match ty with
        | None -> ""
        | Some t -> sprintf " : %s" t
      in
      let body = indent 2 body in
      sprintf "let %s%s =\n%s\n;;\n"
        name opt_ty body

    let func name args body =
      check_lid ~place:"Codegen.Struc.func" name;
      sprintf "let %s %s =\n%s\n;;\n"
        name
        (String.concat " " args)
        (indent 2 body)

    let items body_items =
      String.concat "" &
      List.map (fun item -> item ^ "\n;;\n") body_items

    let module_ name body_items =
      check_uid ~place:"Codegen.Struc.module_" name;
      sprintf "module %s = struct\n%send"
        name
        (indent 2 &
         items body_items
        )

  end

module Record
 =
  struct

    let typedef rname fields =
      check_lid rname ~place:"Codegen.Record.typedef";
      let buf = Buffer.create 100 in
      let fld (n, t) = sprintf "%s : %s" n t in
      begin format_mids
        ~when_0:(fun () -> Buffer.add_string buf "  {}")
        ~when_1:(fld @> bprintf buf "  { %s }\n")
        ~first: (fld @> bprintf buf "  { %s\n")
        ~mid:   (fld @> bprintf buf "  ; %s\n")
        ~last:  (fld @> bprintf buf "  ; %s\n  }\n")
        fields
      end;
      let body = Buffer.contents buf in
      sprintf "type %s =\n%s;;\n" rname body

  end

(* not "Array" to avoid overriding of Array.{get,set} when opening Codegen *)
module Arr
 =
  struct
    let get arr_expr index =
      sprintf "%s.(%s)" arr_expr index
  end

let uid ?(place="Codegen.uid") n =
  check_uid ~place n; n

let line_directive fname lineno =
  sprintf "# %i %S\n" lineno fname

let dummy_line_directive = line_directive "_none_" 0

let codegen_error fname lineno msg =
  eprintf "File %S, line %i:\n%s\n%!" fname lineno msg
