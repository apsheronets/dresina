open Cd_All
open Strings.Latin1
open Printf

let linedirs = true
let linedirs = false
(*
*)

let failwith fmt = Printf.ksprintf Pervasives.failwith fmt
let invalid_arg fmt = Printf.ksprintf Pervasives.invalid_arg fmt
let dbg fmt = Printf.ksprintf (fun s -> Printf.eprintf "DBG: %s\n%!" s) fmt

(*
let ( & ) f x = f x
let ( @> ) f g x = g (f x)
let 
*)

module Cg1 = struct

let check_failed ?place ?arg txt =
  invalid_arg "%s%s"
    (match place with None -> "" | Some p -> p ^ ": ")
    txt
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

let check_poly_constr ~place n =
  if n = ""
  then
    check_failed ~place "poly constructor identifier can't be empty"
  else
    let c = n.[0] in
    if c = '`'
    then
      if ident_ok n
      then ()
      else check_failed ~place ~arg:n
        "sum constructor identifier: bad characters"
    else
      check_failed ~place ~arg:n
        "poly constructor identifier must begin with '`'"


let check_lid ?place n =
  if n = ""
  then
    check_failed ?place ~arg:n "lowercase identifier can't be empty"
  else
    let c = n.[0] in
    if c >= 'a' && c <= 'z' || c = '_'
    then
      if ident_ok n
      then ()
      else check_failed ?place ~arg:n "lowercase identifier: bad characters"
    else
      check_failed ?place ~arg:n
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


end (* Cg1 *)


(* temporary, new codegen. *)

type uid = string
type lid = string
type mod_path = string list
type poly = string

module Cg2
 :
sig

(********* BEGIN SIG ********)

type struc_item
type implem = struc_item list
type typ
type expr
type class_field
type let_ins
type patt
type mod_expr
type mod_type

val expr_lid : string -> expr
val check_lid : ?place:string -> string -> unit
val check_uid : ?place:string -> string -> unit

type arg

module Arg
 :
  sig
    val pos : expr -> arg

    val lab : lid -> expr -> arg
    val opt : lid -> expr -> arg
    
    val lab_self : lid -> arg
    val opt_self : lid -> arg
  end

module Expr
 :
  sig
    val inj : string -> expr
    val user_code : string -> int -> string -> expr

    val local_open : mod_path -> expr -> expr
    val lid : lid -> expr
    val lid_mod : mod_path -> lid -> expr
    val call : lid -> ?lab:(arg list) -> expr list -> expr
    val app : expr -> ?lab:(arg list) -> expr list -> expr
    val app_args : expr -> arg list -> expr
    val call_mod : mod_path -> lid -> ?lab:(arg list) -> expr list -> expr
    val match_ : expr -> (patt * expr) list -> expr
    val try_ : expr -> (patt * expr) list -> expr
    val field : expr -> lid -> expr
    val meth : expr -> lid -> expr list -> expr
    val if_ : expr -> expr -> expr -> expr
    val constr : uid -> expr list -> expr
    val poly : poly -> expr list -> expr
    val seq : expr list -> expr
    val let_in : patt -> expr -> expr -> expr
    val infix : string -> expr -> expr -> expr
    val prepend_let_ins : let_ins -> expr -> expr
    val for_ : lid -> expr -> expr -> expr -> expr
    val fun_ : patt list -> expr -> expr

    val unit : expr
    val int : int -> expr
    val bool : bool -> expr
    val string : string -> expr
    val int64 : int64 -> expr

    val array : expr list -> expr
    val tuple : expr list -> expr
    val list : expr list -> expr

    val object_ : ?self:lid -> class_field list -> expr
    val new_ : ?mod_path:mod_path -> lid -> ?lab:(arg list) -> expr list ->
               expr
    val record : (lid * expr) list -> expr

    val lazy_ : expr -> expr

    val let_module_in : uid -> mod_expr -> expr -> expr
    val ascribe : expr -> typ -> expr

    val linedir : string -> int -> expr -> expr
    val of_body : (string * int * string) -> expr
  end

module Modexpr
 :
  sig
    val prim : ?mod_path:mod_path -> uid -> mod_expr
    val val_ : expr -> mod_type -> mod_expr
    val app : mod_expr -> mod_expr list -> mod_expr
  end

module Modtype
 :
  sig
    val prim : ?mod_path:mod_path -> uid -> mod_type
  end

module Class
 :
  sig
    val val_ : ?mut:bool -> lid -> ?typ:typ -> expr -> class_field
    val method_ : ?pvt:bool -> lid -> patt list -> expr -> class_field
  end

module Typ
 :
  sig
    val unit : typ

    val prim : ?mod_path:mod_path -> lid -> typ
    val param : typ -> typ list -> typ
    val arrow : typ list -> typ
    val sum : (uid * typ list) list -> typ
    val tuple : typ list -> typ
    val record : (lid * typ) list -> typ
  end

module Let_in
 :
  sig
    val empty : let_ins
    val make : patt -> expr -> let_ins
    val append : let_ins -> let_ins -> let_ins
  end

module Patt
 :
  sig
    val unit : patt
    val string : string -> patt
    val int64 : int64 -> patt

    val lid : lid -> patt
    val opt : ?def:expr -> lid -> patt
    val lab : lid -> patt
    val any : patt
    val tuple : patt list -> patt
    val constr : uid -> patt list -> patt
    val poly : poly -> patt list -> patt
    val alt : patt list -> patt

    val ascribe : patt -> typ -> patt

    val inj : string -> patt

    (* not an OCaml pattern really, just "func arg1 .. argN" *)
    val func : lid -> patt list -> patt
  end

module Struc
 :
  sig
    val linedir : string -> int -> struc_item
    val open_ : mod_path -> struc_item

    val expr : lid -> ?typ:typ -> expr -> struc_item
    val func : lid -> patt list -> ?ret_typ:typ -> expr -> struc_item
    (* more generic than expr and func: *)
    val let_ : patt -> ?typ:typ -> expr -> struc_item

    val class_ : lid -> ?pre:let_ins -> patt list -> class_field list ->
                 struc_item
    val module_ : uid -> struc_item list -> struc_item
    val type_ : typ (* prim/param *) -> typ (* definition *) -> struc_item
    val include_ : mod_path -> struc_item
  end

module Implem
 :
  sig
    val to_string : implem -> string
  end

val gensym : lid -> lid

val line_directive : string -> int -> string
val dummy_line_directive : string
val output_codegen_error : string -> int -> string -> unit

val expr_of_int : int -> expr
val expr_of_string : string -> expr
val expr_of_bool : bool -> expr
val expr_of_int64 : int64 -> expr

(********* END SIG **********)

end
 =
struct

(******* BEGIN STRUCT *******)

module I
 :
  sig
    type uid = private string
    val uid : ?place:string -> string -> uid

    type lid = private string
    val lid : ?place:string -> string -> lid

    type mod_path = private uid list
    val mod_path : string list -> mod_path

    type poly = private string
    val poly : string -> poly
  end
 =
  struct
    type uid = string
    let uid ?place s = Cg1.uid ?place s

    type lid = string
    let lid ?place l =
      Cg1.check_lid ?place l;
      l

    type mod_path = uid list
    let mod_path = List.map uid

    type poly = string
    let poly s =
      Cg1.check_poly_constr ~place:"I.poly" s;
      s
  end

open I

type constr = mod_path * uid
type val_mutable =
  | Vmutable
  | Vimmutable
type method_private =
  | Mprivate
  | Mpublic
type arg =
  | Pos of expr
  | Lab of lid * expr option
  | Opt of lid * expr option
and expr =
  | Str of string
  | User_code of string * int * string
  | Local_open of mod_path * expr
  | Lid of lid
  | App of expr * arg list
  | Value_path of mod_path * lid
  | Match of expr * (patt * expr) list
  | Try of expr * (patt * expr) list
  | Lit of string
  | Earray of expr list
  | Etuple of expr list
  | Elist of expr list
  | Erecord of (lid * expr) list
  | Meth of expr * lid
  | If of expr * expr * expr
  | Constr of constr * expr list
  | Poly of poly * expr list
  | Seq of expr list
  | Let_in of patt * expr * expr
  | Op_infix of string * expr * expr
  | Object of lid option (*self*) * class_field list
  | For of lid * expr * expr * expr
  | Fun of patt list * expr
  | Elinedir of string * int * expr
  | Field of expr * lid
  | Let_module_in of uid * mod_expr * expr
  | Eascribe of expr * typ
and class_field =
  | CFval of lid * val_mutable * typ option * expr
  | CFmethod of lid * method_private * patt list * expr
and typ =
  | Tprim of mod_path * lid
  | Tparam of typ * typ list
  | Tarrow of typ * typ
  | Tsum of (uid * typ list) list
  | Ttuple of typ list
  | Trecord of (lid * typ) list
and patt =
  | Pstr of string  (* here: _, (), lid, literals *)
  | Ptuple of patt list
  | Pconstr of uid * patt list
  | Palt of patt list
  | Ppoly of poly * patt list
  | Plab of lid
  | Popt of lid * expr option
  | Pascribe of patt * typ
  | Pfunc of lid * patt list
and mod_expr =
  | Meprim of mod_path * uid
  | Meapp of mod_expr * mod_expr list
  | Meval of expr * mod_type
and mod_type =
  | Mtprim of mod_path * uid
type struc_item =
  | Sval of patt * typ option * expr
      (* patt may be Pstr for values or Pfunc for functions *)
  | Smodule of uid * struc_item list
  | Sclass of lid * patt list
            * expr (* must be [optional let_ins + ] Object [..] *)
  | Sopen of mod_path
  | Slinedir of string * int
  | Stype of typ * typ  (* new type (prim/param), type definition *)
  | Sinclude of mod_path

type let_ins = expr -> expr

type implem = struc_item list

let lid s = Cg1.Expr.lid s
(* let string_of_lid = identity  wtf *)

let check_lid ?place l = ignore (I.lid ?place l)
let check_uid ?place l = ignore (I.uid ?place l)

module Typ
 =
  struct
    let prim ?(mod_path=[]) lid = Tprim (I.mod_path mod_path, I.lid lid)
    let unit = prim "unit"
    let param typ args = assert (args <> []); Tparam (typ, args)
    let arrow args =
      match args with
      | [] -> assert false
      | h :: t ->
          let rec inner h t =
            match t with
            | [] -> assert false
            | [last] -> Tarrow (h, last)
            | m :: t -> Tarrow (h, inner m t)
          in
            inner h t
    let sum constrs =
      Tsum (List.map (fun (c, args) -> (I.uid c, args)) constrs)
    let tuple components = Ttuple components
    let record fields = Trecord (List.map (fun (l, t) -> I.lid l, t) fields)
  end

module Arg
 =
  struct
    let opt l e = Opt (I.lid l, Some e)
    let lab l e = Lab (I.lid l, Some e)
    let opt_self l = Opt (I.lid l, None)
    let lab_self l = Lab (I.lid l, None)
    let pos arg = Pos arg
    let pos_list args = List.map pos args
  end

module Expr
 =
  struct
    let inj s = Str s
    let user_code fname lineno code = User_code (fname, lineno, code)

    let local_open mod_path e = Local_open (I.mod_path mod_path, e)
    let lid l = Lid (I.lid l)
    let lid_mod mod_path l =
      if mod_path = []
      then lid l
      else Value_path (I.mod_path mod_path, I.lid l)
    let app_args f args =
      if args = [] then f else App (f, args)
    let app f ?(lab=[]) args =
      let all_args = lab @ Arg.pos_list args in
      app_args f all_args
    let call_mod mod_path l ?(lab=[]) args =
      app (lid_mod mod_path l) ~lab args
    let call l ?(lab=[]) args = app (lid l) ~lab args
    let match_ e clauses = Match (e, clauses)
    let try_ e with_ = Try (e, with_)
    let field e l = Field (e, I.lid l)
    let meth obj meth args = app (Meth (obj, I.lid meth)) args
    let if_ cond th el = If (cond, th, el)
    let constr c args = Constr ((I.mod_path [], I.uid c), args)
    let poly c args = Poly (I.poly c, args)
    let seq lst = Seq lst
    let let_in patt binding body = Let_in (patt, binding, body)
    let infix op_str e1 e2 = Op_infix (op_str, e1, e2)
    let prepend_let_ins lis e = lis e
    let for_ l f t b = For (I.lid l, f, t, b)
    let fun_ args body = assert (args <> []); Fun (args, body)

    let unit = Lit "()"
    let int i = Lit (Cg1.ml_of_int i)
    let string s = Lit (Cg1.ml_of_string s)
    let bool b = Lit (Cg1.ml_of_bool b)
    let int64 i = Lit (Printf.sprintf "%LiL" i)
    let array items = Earray items
    let tuple items = Etuple items
    let list items = Elist items

    let object_ ?self items = Object (Option.map I.lid self, items)
    let new_ ?(mod_path=[]) cls ?(lab=[]) args =
      App
        ( lid "new"
        , (Pos (lid_mod mod_path cls)) :: lab @ Arg.pos_list args
        )
    let record fv = Erecord (List.map (fun (l, v) -> I.lid l, v) fv)

    let lazy_ e = call "lazy" [e]

    let let_module_in u me body = Let_module_in (I.uid u, me, body)

    let ascribe e t = Eascribe (e, t)

    let linedir fname lineno e = Elinedir (fname, lineno, e)

    let of_body (fname, lineno, bodycode) = linedir fname lineno & inj bodycode
  end

let expr_lid = Expr.lid

module Class
 =
  struct
    let val_ ?(mut=false) lid ?typ expr =
      CFval (I.lid lid, (if mut then Vmutable else Vimmutable), typ, expr)
    let method_ ?(pvt=false) lid args expr =
      CFmethod (I.lid lid, (if pvt then Mprivate else Mpublic), args, expr)
  end

module Let_in
 =
  struct
    let empty = fun e -> e
    let make patt binding = fun e ->
      Expr.let_in patt binding e
    let append lis1 lis2 = fun e ->
      lis1 (lis2 e)
  end

module Patt
 =
  struct
    let any = Pstr "_"
    let unit = Pstr "()"
    let string s = Pstr (Cg1.ml_of_string s)
    let int64 i = Pstr (Printf.sprintf "%LiL" i)
    let lid l = Cg1.check_lid ~place:"Patt.lid" l; Pstr l
    let tuple patts = Ptuple patts
    let constr u args = Pconstr (I.uid u, args)
    let poly u args = Ppoly (I.poly u, args)
    let alt plist = assert (List.length plist >= 2); Palt plist
    let opt ?def l = Popt (I.lid l, def)
    let lab l = Plab (I.lid l)
    let func l args = Pfunc (I.lid l, args)
    let ascribe p t = Pascribe (p, t)
    let inj s = Pstr ("(" ^ s ^ ")")
  end

module Modexpr
 =
  struct
    let prim ?(mod_path = []) u = Meprim (I.mod_path mod_path, I.uid u)
    let val_ e mt = Meval (e, mt)
    let app f args = Meapp (f, args)
  end

module Modtype
 =
  struct
    let prim ?(mod_path = []) u = Mtprim (I.mod_path mod_path, I.uid u)
  end



module Struc
 =
  struct
    let linedir fname lineno = Slinedir (fname, lineno)

    let open_ mp =
      if mp = []
      then invalid_arg "Struc.open_: empty mod_path"
      else
        Sopen (I.mod_path mp)

    let let_ p ?typ body =
      Sval (p, typ, body)

    let expr ident ?typ body =
      let_ (Patt.lid ident) ?typ body

    let func ident args ?ret_typ body =
      let_ (Patt.func ident args) ?typ:ret_typ body

    let class_ ident ?(pre=Let_in.empty) args class_fields =
      Sclass
        ( I.lid ident
        , args
        , Expr.prepend_let_ins pre (Object (None, class_fields))
        )

    let module_ u lst = Smodule (I.uid u, lst)

    let type_ t def = Stype (t, def)

    let include_ mp = Sinclude (I.mod_path mp)

  end

(* copypaste from Cg1 { *)
let line_directive fname lineno =
  if linedirs
  then
    sprintf "# %i %S\n" lineno fname
  else
    ""

let dummy_line_directive = line_directive "_none_" 0

let output_codegen_error fname lineno msg =
  eprintf "File %S, line %i:\n%s\n%!" fname lineno msg
(* } copypaste from Cg1 *)

module Print
 =
  struct
    open PPrintEngine
    open PPrintCombinators

    let h = hardline

    let user_code_output ~linedir_begin ~linedir_end output column code =
      let out_str s = output#substring s 0 (String.length s) in
      output#char '\n';
      begin match linedir_begin with
      | None -> ()
      | Some (fname, lineno) -> out_str (line_directive fname lineno)
      end;
      (* let () = Printf.eprintf "CODE: %S\n%!" code in *)
      out_str code;
      if linedir_end then out_str "\n# 0 _generated_\n";
      blanks output column

    let user_code ~linedir_begin ~linedir_end code =
      custom &
      object
        method requirement = 0
        method pretty output state _indent _flatten =
          user_code_output ~linedir_begin ~linedir_end output state.column code
        method compact output =
          user_code_output ~linedir_begin ~linedir_end output 0 code
      end


    let ( !< ) l = (l : lid :> string)
    let ( !^< ) l = !^ !< l
    let ( !> ) u = (u : uid :> string)
    let ( !^> ) u = !^ !> u
    let ( !>@ ) mp = (mp : mod_path :> string list)

    let tuple docs =
      group &
      ifflat
        (!^"(" ^^ separate (string ", ") docs ^^ !^")")
        (!^"( " ^^ separate (break 0 ^^ !^", ") (List.map align docs) ^^ h
         ^^ !^")")

    (* copypaste! *)
    let array docs =
      group &
      ifflat
        (!^"[| " ^^ separate (string ";") docs ^^ !^" |]")
        (!^"[| " ^^ separate (break 0 ^^ !^"; ") (List.map align docs) ^^ h
         ^^ !^" |]")

    let mp_doc mp = separate dot & List.map string !>@mp
    let doc_path mp doc =
      (* let mp = (mp : I.mod_path :> string list) in *)
      if (mp : I.mod_path :> string list) = []
      then doc
      else mp_doc mp ^^ dot ^^ doc

    let ascribe_docs edoc tdoc =
      group & ifflat
        (parens & edoc ^^ !^" : " ^^ tdoc)
        (align &
         !^"( " ^^ edoc ^^h^^ !^" :" ^^h^^ !^"  " ^^ tdoc ^^h^^ !^")"
        )

    (* precedences of expressions, patterns, types *)
    let ep_dot = 90
    and ep_meth = 85
    and ep_arg = 80
    and ep_app = 80
    and pp_constr = 80
    and ep_object = 78
      (* not documented; but higher than (~-) and lower than func.app. *)
    and ep_comma = 30
    and ep_if = 15
    and ep_semicolon = 10
    and pp_alt = 2
    and pp_out = 0
    and ep_match = 0
    and ep_fun = 0
    and ep_let = 0
    and ep_out = 0

    and tp_app = 10
    and tp_aster = 6
    and tp_arrow = 4
    and tp_out = 0

    let op_desc op =
      (* todo: full *)
      match op with
      | ">>=" -> (`Left, 40)
      | "<-" -> (`Right, 25)
      | _ -> failwith "op_desc: %S" op

    (* [?body:None] for structure items,
       [~body:document] for let-in body.
     *)
    let rec let_in patt ?body ?opt_typ binding =
      group begin
        ifflat
          (!^"let " ^^ patt ^^
             (match opt_typ with
              | None -> (empty : document)
              | Some t -> !^" : " ^^ align (typ tp_out false t)
             ) ^^
             !^" = " ^^ binding ^^
             (if body = None then empty else break 1 ^^ !^"in")
          )
          (!^"let" ^^h^^
           blank 2 ^^ align patt ^^h^^
           (match opt_typ with
            | None -> empty
            | Some t -> !^" : " ^^ align (typ tp_out false t) ^^h
           ) ^^
           !^" = " ^^h^^
           blank 2 ^^ align binding ^^
             (if body = None then empty else h ^^ !^"in")
          )
      end
      ^^ (match body with None -> empty | Some f -> h ^^ f)

    and expr nprec nlower = function
    | Str s -> parens !^s
    | User_code (fname, lineno, code) ->
       user_code ~linedir_begin:(Some (fname, lineno)) ~linedir_end:true code
    | Local_open (mp, e) -> group &
        !^"let open " ^^ mp_doc mp ^^ !^" in" ^^ break 1 ^^
        expr ep_out false e
    | Lid l -> !^<l
    | App (f, args) ->
        assert (args <> []);
        let fdoc = expr ep_arg true f
        and argdocs = List.map (arg ep_arg) args in
        group &
        par nprec nlower ep_app &
          ifflat
          (separate !^" " (fdoc :: argdocs))
          (fdoc ^^ hardline ^^ !^ "  " ^^ align (separate hardline argdocs))
    | Value_path (mp, l) -> doc_path mp !^<l
    | Match (e, cases) ->
        let wth = with_match_cases cases
        and edoc = expr ep_out false e in
        par_be nprec nlower ep_match &
        ifflat
          (!^"match " ^^ edoc ^^ space ^^ wth)
          (!^"match" ^^h^^ !^"  " ^^ align edoc ^^h^^ wth)
    | Try (e, cases) ->
        par_be nprec nlower ep_match &
        let body = expr ep_out false e in
        ifflat
          (!^"try " ^^ body)
          (!^"try" ^^h^^ !^"  " ^^ align body)
        ^^
        break 1 ^^
        with_match_cases cases
    | Lit s -> !^s
    | Earray exprs -> array & List.map (expr ep_semicolon true) exprs
    | Etuple exprs ->
        group &
        let edocs = List.map (expr ep_comma true) exprs in
        let oneline = separate !^", " edocs
        and sep_aligned = separate (h^^ !^", ") & List.map align edocs in
        if need_par ~nprec ~nlower ~cprec:ep_comma
        then
          ifflat
            (parens oneline)
            (!^"( " ^^ sep_aligned ^^h^^ !^")")
        else
          ifflat 
            oneline
            (!^"  " ^^ sep_aligned)
    | Erecord fields_vals ->
        let docs =
          List.map
            (fun (l, e) ->
               !^<l ^^ !^" = " ^^ align (expr ep_semicolon true e)
            )
            fields_vals
        in
        group &
        ifflat
          (braces & separate !^"; " docs)
          (!^"{ " ^^ separate (h^^ !^"; ") (List.map align docs) ^^h^^ !^"}")
    | Elist exprs ->
        let edocs = List.map (expr ep_semicolon true) exprs in
        group &
        ifflat
          (brackets & separate !^"; " edocs)
          (!^"[ " ^^ separate (h^^ !^"; ") (List.map align edocs) ^^h^^ !^"]")
    | Field (recexpr, l) ->
        par nprec nlower ep_dot &
          expr ep_dot false recexpr ^^ dot ^^ !^<l
    | Meth (objexpr, meth) ->
        let objdoc = expr ep_meth false objexpr in
        par nprec nlower ep_meth &
        ifflat
          (objdoc ^^ !^"#" ^^ !^<meth)
          (objdoc ^^h^^ !^"#" ^^ !^<meth)
    | If (cond, th, el) ->
        let cond_doc = expr ep_if false cond
        and th_doc = expr ep_if false th
        and el_doc = expr ep_if false el in
        group &
        par_be nprec nlower ep_if &
        ifflat
          (!^"if " ^^ cond_doc ^^ !^" then " ^^ th_doc ^^ !^" else " ^^ el_doc)
          (!^"if" ^^
             (group & ifflat
                (space ^^ cond_doc)
                (h^^ !^"  " ^^ align cond_doc)
             ) ^^
           h^^
           !^"then" ^^h^^ !^"  " ^^ align th_doc ^^h^^
           !^"else" ^^h^^ !^"  " ^^ align el_doc
          )
    | Constr ((mp, constr), args) ->
        constr_args nprec nlower
          (doc_path mp !^>constr) (List.map (expr ep_arg true) args)
    | Poly (constr, args) ->
        constr_args nprec nlower
          !^(constr :> string) (List.map (expr ep_arg true) args)
    | Seq exprs ->
        let edocs = List.map (expr ep_semicolon true) exprs in
        par_be nprec nlower ep_semicolon &
        ifflat
          (separate !^"; " edocs)
          (separate (!^";" ^^h) edocs)
    | Let_in (p, bindings, body) ->
        par_be nprec nlower ep_let &
        let patt_doc = patt pp_out false p in
        let_in patt_doc (expr ep_out false bindings)
          ~body:(expr ep_out false body)
    | Let_module_in (u, mexpr, body) ->
        let patt_doc = !^"module " ^^ !^>u
        and medoc = mod_expr mexpr in
        let_in patt_doc medoc
          ~body:(expr ep_out false body)
    | Op_infix (op, e1, e2) ->
        let (op_assoc, op_prec) = op_desc op in
        let (llower, rlower) =
          match op_assoc with
          | `Left -> false, true
          | `Right -> true, false
        in
        let edoc1 = expr op_prec llower e1
        and edoc2 = expr op_prec rlower e2 in
        par nprec nlower op_prec &
        ifflat
          (edoc1 ^^ space ^^ !^op ^^ space ^^ edoc2)
          (align edoc1 ^^h^^ !^op ^^h^^ align edoc2)
    | Object (opt_self, cfields) ->
        par_be nprec nlower ep_object &
        !^"object" ^^
        (match opt_self with
         | None -> empty
         | Some l -> parens !^<l
        ) ^^
        h^^
        !^"  " ^^
        align (concat & List.map class_field cfields) ^^
        !^"end"
    | For (l, efrom, eto, body) ->
        let bodydoc = expr ep_out false body in
        group &
        !^"for " ^^ !^<l ^^ !^" = " ^^
        (expr ep_out false efrom) ^^ !^" to " ^^
        (expr ep_out false eto) ^^
        ifflat
          (!^" do " ^^ bodydoc ^^ !^" done")
          (h^^ !^"do" ^^h^^ !^"  " ^^ align bodydoc ^^h^^ !^"done")
    | Fun (args, body) ->
        let bodydoc = expr ep_fun false body
        and argdocs = List.map (patt pp_out false) args in
        !^"fun " ^^
        ifflat
          (separate space argdocs ^^ !^" -> " ^^ bodydoc)
          (nest 2 (group & separate (break 1) argdocs ^^
           break 1 ^^ !^"->" ^^h^^ !^"  " ^^ align bodydoc)
          )
    | Elinedir (fname, lineno, e) ->
        let edoc = expr nprec nlower e in
        if linedirs
        then
          user_code ~linedir_begin:(Some (fname, lineno)) ~linedir_end:false ""
          ^^ edoc ^^
          user_code ~linedir_begin:None ~linedir_end:true ""
        else
          edoc
    | Eascribe (e, t) ->
        let edoc = expr ep_out false e
        and tdoc = typ tp_out false t in
        ascribe_docs edoc tdoc

    and mod_expr = function
    | Meprim (mp, u) -> doc_path mp !^>u
    | Meapp (me, args) ->
        let medoc = mod_expr me
        and argdocs = List.map mod_expr args in
        group &
        medoc ^^ flow (break 0) (List.map parens argdocs)
    | Meval (e, mt) ->
        let edoc = expr ep_out false e
        and tdoc = mod_type mt in
        ascribe_docs
          (!^"val " ^^ edoc)
          tdoc

    and mod_type = function
    | Mtprim (mp, u) ->
        doc_path mp !^>u

    and with_match_cases cases =
      let casedocs = List.map match_case cases in
      ifflat
        (!^"with " ^^ separate !^" | " casedocs)
        (!^"with" ^^h^^
         separate h
           (List.map
             (fun d -> !^"| " ^^ align (group d))
             casedocs
           )
        )

    and match_case (p, e) =
      let pdoc = patt pp_out false p
      and edoc = expr ep_match true e in
      ifflat
        (pdoc ^^ !^" -> " ^^ edoc)
        (pdoc ^^ !^" ->" ^^h^^ !^"    " ^^ align edoc)

    and arg nprec = function
    | Pos e -> expr ep_arg true e
    | Lab (l, eopt) -> !^("~" ^ !<l) ^^ arg_lab_val nprec eopt
    | Opt (l, eopt) -> !^("?" ^ !<l) ^^ arg_lab_val nprec eopt

    and arg_lab_val nprec = function
    | None -> empty
    | Some e -> !^":" ^^ par nprec false ep_arg (expr ep_arg true e)

    and patt nprec nlower : patt -> document = function
    | Pstr s -> !^s
    | Ptuple patts -> tuple & List.map (patt pp_constr false) patts
    | Pconstr (u, patts) ->
        constr_args nprec nlower !^>u (List.map (patt pp_constr false) patts)
    | Palt patts -> par nprec nlower pp_alt &
        let pattdocs = List.map (patt pp_alt false) patts in
        ifflat
          (separate !^" | " pattdocs)
          (align & separate (break 1 ^^ !^"| ") pattdocs)
    | Ppoly (p, patts) ->
        constr_args nprec nlower
          !^(p :> string) (List.map (patt pp_constr false) patts)
    | Plab l -> !^"~" ^^ !^<l
    | Popt (l, None) -> !^"?" ^^ !^<l
    | Popt (l, Some e) -> !^"?(" ^^ !^<l ^^ !^" = " ^^
        expr ep_out false e ^^ !^")"
    | Pascribe (p, t) ->
        ascribe_docs
          (patt ep_out false p)
          (typ tp_out false t)
    | Pfunc (l, args) ->
        group & align &
        !^<l ^^ space ^^
        let argdocs = List.map (patt pp_out true) args in
        ifflat
          (separate space argdocs)
          (nest 2 & flow (break 1) argdocs)

    and constr_args nprec nlower constr args : document =
      par nprec nlower ep_app &
      constr ^^
      match args with
      | [] -> empty
      | h :: [] -> space ^^ h
      | _ ->
          let tup = tuple args in
          space ^^
          (group & ifflat tup (h^^ !^"  " ^^ align tup))

    and typ nprec nlower = function
    | Tprim (mp, ident) ->
        (if !>@mp = [] then empty else mp_doc mp ^^ !^".") ^^ !^< ident
    | Tparam (ty, params) ->
        begin match params with
        | [] -> assert false
        | [a] -> typ tp_app false a
        | _ -> tuple (List.map (typ tp_out false) params)
        end
        ^^ break 1 ^^ typ tp_out false ty
    | Tarrow (cod, dom) ->
        par nprec nlower tp_arrow &
          typ tp_arrow true cod ^^
          break 1 ^^ !^"-> " ^^
          typ tp_arrow false dom
    | Tsum constrs ->
        let constrdocs = List.map
          (fun (u, args) ->
             !^>u ^^
             (if args = []
              then empty
              else
                let argdocs = List.map (typ tp_aster true) args in
                align &
                !^" of " ^^
                (group %%< ifflat)
                  (separate !^" * " argdocs)
                  (separate (h^^ !^"  * ") & List.map align argdocs)
             )
          )
          constrs
        in
        group &
        ifflat
          (separate !^" | " constrdocs)
          (align & separate h &
           List.map (fun c -> !^"| " ^^ c) constrdocs
          )
    | Ttuple components ->
        let docs = List.map (typ tp_aster true) components in
        par nprec nlower tp_aster &
        ifflat
          (separate !^" * " docs)
          (!^"  " ^^ separate (h^^ !^"* ") (List.map align docs))
    | Trecord fields ->
        let fielddocs =
          List.map
            (fun (l, t) ->
               let tdoc = typ tp_out false t in
               !^<l ^^ !^" : " ^^
               group (ifflat tdoc (align tdoc))
            )
            fields
        in
        group &
        ifflat
          (!^"{ " ^^ separate !^"; " fielddocs ^^ !^" }")
          (align &
           !^"{ " ^^ separate (h^^ !^"; ") (List.map align fielddocs) ^^
           !^"}"
          )

    and class_field = function
    | CFval (l, mut, opt_typ, body) ->
        let bodydoc = expr ep_out false body in
        begin group &
        !^"val " ^^
        (match mut with Vmutable -> !^"mutable " | Vimmutable -> empty) ^^
        !^<l ^^
        (match opt_typ with
         | None -> empty
         | Some t ->
             let tdoc = typ tp_out false t in
             ifflat
               (!^" : " ^^ tdoc)
               (h^^ !^" : " ^^ align tdoc ^^h)
        ) ^^
        ifflat
          (!^" = " ^^ bodydoc)
          (h^^ !^" = " ^^ align bodydoc)
        end
        ^^h
    | CFmethod (l, pvt, args, body) ->
        let argdocs = List.map (patt pp_out false) args
        and bodydoc = expr ep_out false body in
        begin group &
        !^"method " ^^
        (match pvt with Mprivate -> !^"private " | Mpublic -> empty) ^^
        !^<l ^^ space ^^
        ifflat
          (concat & List.map (fun a -> a ^^ space) argdocs)
          (nest 2 & concat & List.map (fun a -> a ^^ break 1) argdocs) ^^
        !^"=" ^^
        ifflat (space ^^ bodydoc) (h^^ !^"  " ^^ align bodydoc)
        end
        ^^h

    and need_par ~nprec ~nlower ~cprec =
      (nlower && cprec <= nprec)
      || (not nlower && cprec < nprec)

    and par nprec nlower cprec cdoc =
      group &
      if need_par ~nprec ~nlower ~cprec
      then
        ifflat
          (parens cdoc)
          (!^"( " ^^ align cdoc ^^ h ^^ !^") ")
      else
        cdoc

    (* copypaste! *)
    and par_be nprec nlower cprec cdoc =
      group &
      if need_par ~nprec ~nlower ~cprec
      then
        ifflat
          (parens cdoc)
          (!^"begin" ^^h^^ !^"  " ^^ align cdoc ^^h^^ !^"end")
      else
        cdoc


    let rec struc_item =
      let eosi = terminate (h ^^ !^";;" ^^h^^h) in
      function
      | Sclass (l, args, body) -> eosi &
          !^"class " ^^
          (nest 2 & flow (break 1) &
           !^<l :: List.map (patt pp_out false) args @ [!^"="]
          ) ^^h^^
          !^"  " ^^ align (expr pp_out false body)

      | Smodule (ident, items) -> eosi &
          !^"module " ^^ !^ !> ident ^^h^^
          !^" =" ^^h^^
          !^"  struct" ^^h^^
          h^^
          !^"    " ^^
             (align & concat & List.map struc_item items) ^^h^^
          !^"  end"
      | Sopen mp -> eosi & string & "open " ^ String.concat "." !>@mp
      | Sinclude mp -> eosi & string & "include " ^ String.concat "." !>@mp
      | Sval (p, opt_typ, binding) -> eosi &
          let let_patt = patt pp_out false p in
          let_in let_patt ?opt_typ (expr 0 false binding)
      | Slinedir (fname, lineno) ->
          if linedirs
          then
            user_code ~linedir_begin:(Some (fname, lineno)) ~linedir_end:false
              ""
          else empty
      | Stype (t, def) -> eosi &
          group &
          !^"type " ^^ typ tp_out false t ^^ !^" =" ^^ break 1 ^^
          typ tp_out false def

    let implem im = concat & List.map struc_item im
  end

module Implem
 =
  struct
    let to_string si =
      let b = Buffer.create 1000 in
      ( PPrintEngine.ToBuffer.pretty 1.0 78 b (Print.implem si)
      ; Buffer.contents b
      )
  end


let gensym = Cg1.gensym

let expr_of_int = Expr.int
let expr_of_string = Expr.string
let expr_of_bool = Expr.bool
let expr_of_int64 = Expr.int64


(******** END STRUCT ********)

end (* Cg2 *)
