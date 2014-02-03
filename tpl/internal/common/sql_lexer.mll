(* very dumb lexing for now, to make "col = @var" work.
   todo: strings, function calls.
 *)


let space = [ ' ' '\t' '\n' '\r' ]
let not_space = ['\000'-'\255'] # space
let ident_body = [ 'a'-'z' 'A'-'Z' '0'-'9' '_' '\'' ]
let ident = [ 'a'-'z' '_' ] ident_body*


rule main = parse
  space*
    {
      loop [] lexbuf
    }


and loop acc = parse

  space
    {
      assert false
    }

| '@' (ident as i)
  (space*
     ':' space*
     (("option" as opt) space*)?
     (ident as t)
  )?
  (space* as s)
    {
      let ty_nul_opt =
        match t, opt with
        | None, None -> None
        | Some t, None -> Some (t, `Notnull)
        | Some t, Some _opt -> Some (t, `Nullable)
        | None, Some _opt -> assert false
      in
      loop (`Var (i, ty_nul_opt, s) :: acc) lexbuf
    }

(* todoc: escaping *)
| "\\@" (not_space* as n) (space* as s)
    {
      loop (`Str ("@" ^ n, s) :: acc) lexbuf
    }

| (not_space+ as ns) (space* as s)
    {
      loop (`Str (ns, s) :: acc) lexbuf
    }

| eof
    {
      let acc =
        match acc with
        | `Var (n, t, _spc) :: tl -> `Var (n, t, "") :: tl
        | `Str (ns, _spc) :: tl -> `Str (ns, "") :: tl
        | [] -> []
      in
      List.rev acc
    }
