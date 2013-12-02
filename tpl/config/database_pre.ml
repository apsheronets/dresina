type context = string option ref

let connection1b db_kind body ctx =
  let ci = Expr.modqual ("Dbi_" ^ db_kind) "conn_info" in
  ctx := Some begin Printf.sprintf
    "let conn_info = new %s\n%s\n();;\n"
      ci body
  end
