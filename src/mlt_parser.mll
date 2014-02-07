{

module Cg = Codegen

let error fname lineno fmt =
  Printf.ksprintf
    (fun s -> failwith (Printf.sprintf
       "file %S, line %i: %s" fname lineno s)
    )
    fmt

let ml_code txt = Cg.Sum.constr "Ml" [ Cg.Lit.string txt ]

(* returns code that will be placed inside Dir constructor *)
let code_of_directive dirtype dir args =
  let pos_args_count = List.length (List.filter
      (function
       | `Pos _ -> true
       | `Body _ | `Lab (_, _) -> false
      )
      args
    )
  in
  let func_name =
    match dirtype with
    | `Strings -> Printf.sprintf "%s%i" dir pos_args_count
    | `With_body -> Printf.sprintf "%s%ib" dir pos_args_count
  in
  if args <> []
  then
    let args_code = List.map
      (function
       | `Pos code | `Body code -> code
       | `Lab (label, code) -> "~" ^ label ^ ":(" ^ code ^ ")"
      )
      args
    in
    Cg.Expr.call func_name args_code
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

let ret_string str = Some (Cg.Sum.constr "`Str" [Cg.Lit.string str])
}


let space = [ '\x20' '\x09' ]
let eol = '\r'? '\n'
let eol_char = [ '\n' '\r' ]
let dir = space* '%' space*
let dir_comment = space* '#' space*
let anychar = [ '\000' - '\255' ]
let linechar = anychar # eol_char
let linechar_no_quote = linechar # [ '"' ]
let not_directive_beginning =
  (
     (linechar # ['#' '%'] # space)
   |
     ( '#'
       space*
       (linechar # ['%'] # space)
     )
  )
let ident_re = [ 'a' - 'z' ] [ 'a' - 'z' 'A' - 'Z' '0' - '9' '_' ]*
let unquoted_string_char =
  [ '\033' - '\126' ] # [ '\'' '"' '[' ']' '{' '}' '(' ')' ]
let unquoted_string_char_no_tilde = unquoted_string_char # [ '~' ]

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
          (code_of_directive
             `With_body
             dir_name
             (dir_args @ [`Body body_ml])
          )
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


and dir_args rev_acc fname lineno = parse
  (* в начале буфера пробелов нет. *)

| ""
    {
      match dir_arg fname lineno lexbuf with
      | `Eoa -> (lineno + 1), List.rev rev_acc
      | (`Pos _ | `Lab (_, _)) as a ->
          dir_args (a :: rev_acc) fname lineno lexbuf
    }

(* возвращает:
   `Pos код - позициональный аргумент,
   `Lab (label, код) - опциональный аргумент "~label:строка",
   `Eoa - конец аргументов
 *)
and dir_arg fname lineno = parse

  '~'
    {
      let label = ident lexbuf in
      let () = labelled_colon fname lineno lexbuf in
      match dir_arg_no_label fname lineno lexbuf with
      | None -> failwith "labelled argument must have some value"
      | Some ml_code -> `Lab (label, ml_code)
    }

| ""
    {
      match dir_arg_no_label fname lineno lexbuf with
      | None -> `Eoa
      | Some ml_code -> `Pos ml_code
    }


and dir_arg_no_label fname lineno = parse
  (* должен съесть все пробелы после себя *)

  eol | eof
    {
      None
    }

| ((unquoted_string_char_no_tilde unquoted_string_char*) as str) space*
    {
      ret_string str
    }

| '"'
    {
      let str = quoted_string fname lineno lexbuf in
      ret_string str
    }

| '[' space*
    {
      Some (list fname lineno [] lexbuf)
    }

| ']'
    {
      error fname lineno "mismatched ']'"
    }

| space
    {
      assert false
    }

| ""
    {
      let ch = peek_char lexbuf in
      error fname lineno "error parsing directive arguments on character %S" ch
    }

and list fname lineno rev_acc = parse

  ']' space*
    {
      Cg.Sum.constr "`List" [Cg.Expr.list (List.rev rev_acc)]
    }

| ""
    {
      match dir_arg_no_label fname lineno lexbuf with
      | None ->
          error fname lineno "list must be finished before end-of-{line,file}"
      | Some elt ->
          let rev_acc = elt :: rev_acc in
          list fname lineno rev_acc lexbuf
    }


and peek_char = parse
  anychar as c
    { String.make 1 c }
| ""
    { "<eof>" }

and labelled_colon fname lineno = parse

  ':' space*
    {
      ()
    }

| ""
    {
      error fname lineno "expected ':' after '~label'"
    }

(* must eat closing quote and spaces after it.
   escaping must be somewhere here.
 *)
and quoted_string fname lineno = parse

  (linechar_no_quote* as str) '"' space*
    {
      str
    }

| linechar_no_quote* (eol | eof)
    {
      error fname lineno "quote must be closed before end-of-{line,file}"
    }

| ""
    {
      assert false
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
