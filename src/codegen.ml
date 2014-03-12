open Cd_All
open Strings.Latin1
open Printf

let failwith fmt = Printf.ksprintf Pervasives.failwith fmt
let invalid_arg fmt = Printf.ksprintf Pervasives.invalid_arg fmt
let dbg fmt = Printf.ksprintf (fun s -> Printf.eprintf "DBG: %s\n%!" s) fmt

(*
let ( & ) f x = f x
let ( @> ) f g x = g (f x)
let 
*)


let check_failed ~place ?arg txt =
  invalid_arg "%s: %s%s" place txt
    (match arg with None -> "" | Some x -> sprintf " (argument: %S)" x)

(* checks from offset 1, first char is checked in caller *)
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
      else check_failed ~place ~arg:n "uppercase identifier: bad characters"
    else
      check_failed ~place ~arg:n
        "uppercase identifier must begin with 'A'..'Z'"

let check_sum_constr ~place n =
  if n = ""
  then
    check_failed ~place "sum constructor identifier can't be empty"
  else
    let c = n.[0] in
    if (c >= 'A' && c <= 'Z') || c = '`'
    then
      if ident_ok n
      then ()
      else check_failed ~place ~arg:n
        "sum constructor identifier: bad characters"
    else
      check_failed ~place ~arg:n
        "sum constructor identifier must begin with 'A'..'Z' or '`'"


let check_lid ~place n =
  if n = ""
  then
    check_failed ~place ~arg:n "lowercase identifier can't be empty"
  else
    let c = n.[0] in
    if c >= 'a' && c <= 'z' || c = '_'
    then
      if ident_ok n
      then ()
      else check_failed ~place ~arg:n "lowercase identifier: bad characters"
    else
      check_failed ~place ~arg:n
        "lowercase identifier must begin with 'a'..'z' or '_'"

module Lit
 =
  struct
    let string s = "\"" ^ String.escaped s ^ "\""
    let int = string_of_int
    let bool = function
      | true -> "true"
      | false -> "false"
  end

(* for ml_of_* : *)
let ml_of_string = Lit.string
let ml_of_int = Lit.int
let ml_of_bool = Lit.bool

let line_is_blank l =
  let len = String.length l in
  let rec loop i =
    if i = len
    then true
    else
      let c = l.[i] in
      if c = '\n'
      then assert false
      else
        if c = '\x20' || c = '\x09'
        then loop (i + 1)
        else false
  in
    loop 0

let indent ind ml =
  if ind = 0
  then ml
  else
    let () = assert (ind > 0) in
    let pre = String.make ind '\x20' in
    ml |>
    String.split_exact ( ( = ) '\n' ) |>
    List.map
      (fun line ->
         if line_is_blank line || line.[0] = '#' (* line directive *)
         then line
         else pre ^ line
      ) |>
    String.concat "\n"


(* пусть остаётся для списков как есть. *)
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

let has_first_n_spaces n str =
  String.length str >= n &&
  (let rec loop i =
     if i = n
     then true
     else str.[i] = ' ' && loop (i + 1)
   in
     loop 0
  )

let format_multiline ~tokbegin ~tokend ~tokmid lst =
  match lst with
  | [] -> tokbegin ^ " " ^ tokend ^ "\n"
  | h :: t ->
      let ind = 1 + max (String.length tokbegin) (String.length tokmid) in
      let rec inner is_first h t =
        let indented = indent ind h in
        let res =
          let (first_line, sep, rest) =
            String.split_by_first ((=) '\n') indented in
          let (tok, len) =
            if is_first
            then (tokbegin, String.length tokbegin)
            else (tokmid, String.length tokmid)
          in
            if first_line = ""
            then tok ^ sep ^ rest
            else begin
              assert (has_first_n_spaces (len + 1) first_line);
              (String.blit_copy
                 ~src:tok ~src_pos:0
                 ~dst:first_line ~dst_pos:0
                 ~len
              ) ^ sep ^ rest
            end
        in
        res ::
        (match t with
         | h' :: t' -> inner false h' t'
         | [] -> [tokend ^ "\n"]
        )
      in
        String.concat "\n" & inner true h t


module Tuple
 =
  struct
    let check_arity ?(one=false) ~fn args =
      if List.length args > (if one then 0 else 1)
      then ()
      else
        invalid_arg
          "Codegen.Tuple.%s: must have %s one type argument" fn
          (if one then "at least" else "more than")

    let constr ?(one=false) ?(newlines = false) args =
      check_arity ~one args ~fn:"constr";
      if newlines
      then
        format_multiline ~tokbegin:"(" ~tokmid:"," ~tokend:")" args
      else
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

    (* extended to poly variants *)
    let constr ?(newlines=false) constr_name args =
      check_sum_constr ~place:"Codegen.Sum.constr" constr_name;
      sprintf "%s%s"
        constr_name
        (match args with
         | [] -> ""
         | _ -> " " ^ Tuple.constr ~newlines ~one:true args
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

    let call_gen__ ?(newlines=false) func_name args =
      let args =
        List.map
          (fun arg ->
             if arg = ""
             then invalid_arg "Expr.call: empty argument"
             else
               if arg.[0] = '~' || arg.[0] = '?' || arg = "()"
               then arg
             else
               if newlines
               then
                 match String.split_exact ((=) '\n') arg with
                 | [] -> assert false
                 | [oneline] -> sprintf "(%s)" oneline
                 | lines -> format_multiline
                     ~tokbegin:"(" ~tokend:")" ~tokmid:"" lines
               else sprintf "(%s)" arg
          ) args
      in
      if newlines
      then
        sprintf "(%s\n%s\n)\n" func_name & String.concat "\n" &
          List.map (indent 3) args
      else
        sprintf "(%s %s)" func_name & String.concat " " args

    (* can call non-qualified functions only; without "Module." parts *)
    let call ?(newlines=false) func_name args =
      check_lid ~place:"Codegen.Expr.call" func_name;
      if args = []
      then invalid_arg
        "Codegen.Expr.call: can't call function without arguments"
      else
        call_gen__ ~newlines func_name args

    (* calls anything specified in [func] argument *)
    let call_gen ?(newlines=false) func args =
      if args = []
      then invalid_arg
        "Codegen.Expr.call_gen: can't call function without arguments"
      else
        call_gen__ ~newlines (sprintf "(%s)" func) args

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
          sprintf "begin\n%s\nend\n"
            (indent 2 & String.concat ";\n" expr_list)

    let for_ var_ from_ to_ body_seq =
      sprintf "for %s = %s to %s do\n%s\ndone"
        (lid var_) from_ to_ (indent 2 & String.concat ";\n" body_seq)

    let if_ cond th el =
      sprintf "if (%s)\nthen begin\n%s\nend else begin\n%s\nend\n"
        cond
        (indent 2 th)
        (indent 2 el)

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
      sprintf "let %s%s =\n%s\n"
        name opt_ty body

    let func ?(arg_per_line=false) ?ret_ty name args body =
      check_lid ~place:"Codegen.Struc.func" name;
      let ret_ty_txt =
        match ret_ty with
        | None -> ""
        | Some t -> " : " ^ t ^ (if arg_per_line then "\n" else "")
      in
      if arg_per_line
      then
        sprintf "let %s\n%s%s  =\n%s\n"
          name
          (String.concat "" &
           List.map (fun a -> "  " ^ a ^ "\n") args
          )
          ret_ty_txt
          (indent 4 body)
      else
        sprintf "let %s %s%s =\n%s\n"
          name
          (String.concat " " args)
          ret_ty_txt
          (indent 2 body)

    let items body_items =
      String.concat "" &
      List.map (fun item -> item ^ "\n;;\n") body_items

    let module_ name body_items =
      check_uid ~place:"Codegen.Struc.module_" name;
      sprintf "module %s = struct\n%send\n"
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

    let constr ?(newlines=false) elements =
      if newlines
      then
        format_multiline ~tokbegin:"[|" ~tokmid:" ;" ~tokend:" |]" elements
      else
        sprintf "[| %s |]" &
        String.concat " ; " elements
  end

let uid ?(place="Codegen.uid") n =
  check_uid ~place n; n

let method_ ?(pvt=false) name args body =
  sprintf "method%s %s %s =\n%s\n"
    (if pvt then " private" else "")
    name
    (String.concat " " args)
    (indent 2 body)

let line_directive fname lineno =
  sprintf "# %i %S\n" lineno fname

let dummy_line_directive = line_directive "_none_" 0

let codegen_error fname lineno msg =
  eprintf "File %S, line %i:\n%s\n%!" fname lineno msg

(* strips directive from beginning of [txt], if it's present *)
let strip_line_directive txt =
  if String.length txt = 0
  then txt
  else
    if txt.[0] = '#'
    then
      let (_line_dir, _newline, rest) =
        String.split_by_first ( ( = ) '\n' ) txt
      in
        rest
    else txt

(* utility functions for mlt processing *)
module Mlt
 =
  struct

    type mlt_val =
      [ `Str of string
      | `List of (mlt_val list)
      ]

    let expect_string place (v : mlt_val) =
      match v with
      | `Str x -> x
      | `List _ -> failwith "argument %s must be string, not a list" place

    let expect_string_opt place (v : mlt_val option) =
      match v with
      | None -> None
      | Some x -> Some (expect_string place x)

    let string_args1 f a ctx =
      f
        (expect_string "no.1" a)
        ctx

    let string_args2 f a b ctx =
      f
        (expect_string "no.1" a)
        (expect_string "no.2" b)
        ctx

    let string_args3 f a b c ctx =
      f
        (expect_string "no.1" a)
        (expect_string "no.2" b)
        (expect_string "no.3" c)
        ctx

    let string_args4 f a b c d ctx =
      f
        (expect_string "no.1" a)
        (expect_string "no.2" b)
        (expect_string "no.3" c)
        (expect_string "no.4" d)
        ctx

    let string_args5 f a b c d e ctx =
      f
        (expect_string "no.1" a)
        (expect_string "no.2" b)
        (expect_string "no.3" c)
        (expect_string "no.4" d)
        (expect_string "no.5" e)
        ctx

    let bool_of_string ~place = function
    | "true" -> true
    | "false" -> false
    | str -> failwith "%s expected to be \"true\" or \"false\", not \"%s\""
        place str

  end

let syms_generated = Hashtbl.create 7
exception Sym of string

let gensym base =
  try
    let check suf =
      let s = base ^ suf in
      if Hashtbl.mem syms_generated s
      then ()
      else begin
        Hashtbl.add syms_generated s ();
        raise (Sym s)
      end
    in
    check "";
    let rec loop i =
      let suf = string_of_int i in
      check suf;
      loop (i + 1)
    in
      loop 2
  with Sym s -> s
