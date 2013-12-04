{

module Cg = Codegen

let ml_code txt = Cg.Sum.constr "Ml" [ Cg.Lit.string txt ]

(* returns code that will be placed inside Dir constructor *)
let code_of_directive dirtype dir args_code =
  let args_count = List.length args_code in
  let func_name =
    match dirtype with
    | `Strings -> Printf.sprintf "%s%i" dir args_count
    | `With_body -> Printf.sprintf "%s%ib" dir (args_count - 1)
  in
  if args_count > 0
  then Cg.Expr.call func_name args_code
  else Cg.Expr.lid func_name

let dir_code fname lineno code =
  let line_dir = "\n" ^ Cg.line_directive fname lineno in
  Cg.Sum.constr "Dir"
    [
        "(dir_with_loc " ^ Cg.Lit.string fname ^ " " ^ Cg.Lit.int lineno
      ^ line_dir
      ^ code
      ^ "\n)"
    ]

}


let space = [ '\x20' '\x09' ]
let eol = '\r'? '\n'
let dir = space* '%' space*
let dir_comment = space* '#' space*
let linechar = [ '\000' - '\255' ] # [ '\n' '\r' ]
let not_directive_beginning =
  (
     (linechar # ['#' '%'])
   |
     ( '#'
       space*
       (linechar # ['%'] # space)
     )
  )
let ident_re = [ 'a' - 'z' ] [ 'a' - 'z' 'A' - 'Z' '0' - '9' '_' ]*
let unquoted_string_char =
  [ '\033' - '\126' ] # [ '\'' '"' '[' ']' '{' '}' '(' ')' ]
let unquoted_string_re = unquoted_string_char+

rule file rev_acc fname lineno = parse
  eof
    {
      List.rev rev_acc
    }

| (dir_comment as comm)? dir
    {
      let (lineno, code) = directive fname lineno lexbuf in
      (* Printf.eprintf "dir res: '%s'\n" code; *)
      let rev_acc =
        match comm with
        | None -> code :: rev_acc
        | Some _ -> rev_acc
      in
      file rev_acc fname lineno lexbuf
    }

| ""
    {
      let buf = Buffer.create 100 in
      Buffer.add_string buf (Cg.line_directive fname lineno);
      let lineno = ml_text lineno buf lexbuf in
      let code = ml_code (Buffer.contents buf) in
      (* Printf.eprintf "ml res: '%s', new lineno=%i\n" code lineno; *)
      file (code :: rev_acc) fname lineno lexbuf
    }


and ml_text lineno buf = parse

  eof | ""
    {
      lineno
    }

| space* (not_directive_beginning linechar*)? (eol | eof)
    {
      let line = Lexing.lexeme lexbuf in
      (* Printf.eprintf "ml_text: %S\n%!" line; *)
      Buffer.add_string buf line;
      ml_text (lineno + 1) buf lexbuf
    }


and directive fname lineno = parse
  (* "  %  " is already consumed *)

  "begin" space*
    {
      let begin_dir_lineno = lineno in
      let (lineno, dir_name, dir_args) =
        directive_no_body fname lineno lexbuf in
      let buf = Buffer.create 100 in
      Buffer.add_string buf (Cg.line_directive fname lineno);
      let lineno = ml_text lineno buf lexbuf in
      let body_ml = Cg.Lit.string (Buffer.contents buf) in
      let lineno = body_end lineno lexbuf in
      ( lineno
      , dir_code fname begin_dir_lineno
          (code_of_directive `With_body dir_name (dir_args @ [body_ml]))
      )
    }

| "end" space*
    {
      failwith "mismatched '% end' directive"
    }

| ""
    {
      let begin_dir_lineno = lineno in
      let (lineno, dir_name, dir_args) =
        directive_no_body fname lineno lexbuf in
      ( lineno
      , dir_code fname begin_dir_lineno
          (code_of_directive `Strings dir_name dir_args)
      )
    }


and directive_no_body fname lineno = parse

  ""
    {
      let dir_name = ident lexbuf in
      let (lineno, dir_args) = dir_args [] fname lineno lexbuf in
      (lineno, dir_name, dir_args)
    }


and ident = parse

  (ident_re as id) space*
    {
      id
    }

| ""
    {
      failwith "expected identifier"
    }


(* возвращает код, представляющий аргумент *)
and dir_args rev_acc fname lineno = parse
  (* в начале буфера пробелов нет.  тут должны скушать eol обязательно. *)

  eol | eof
    {
      (lineno + 1), List.rev rev_acc
    }

| (unquoted_string_re as str) space*
    {
      dir_args (Cg.Lit.string str :: rev_acc) fname lineno lexbuf
    }

| space
    {
      assert false
    }

| ""
    {
      failwith (Printf.sprintf
        "file %S, line %i: error parsing directive arguments"
        fname lineno
      )
    }


and body_end lineno = parse

  dir "end" space* (eol | eof)
    {
      lineno + 1
    }

| dir
    {
      failwith "directive body can't contain nested directives"
    }

| eof
    {
      failwith "expected '% end', found eof"
    }

| ""
    {
      assert false
    }
