open Printf

let psql_cmd ~user ~host ~port_opt = sprintf
  "psql -h %s -U %s -d postgres %s"
  (Filename.quote host)
  (Filename.quote user)
  (match port_opt with
   | None -> ""
   | Some p -> "-p " ^ Filename.quote p
  )

let run_sql ~user ~host ~port_opt ~sql =
  if user = "postgres"
  then
    Printf.printf
      "NOTE: if you are running this command not under 'postgres' OS user,\n\
      \      you will be asked for 'postgres' database user password.\n%!"
  else ();
  let psql_ch = Unix.open_process_out (psql_cmd ~user ~host ~port_opt) in
  output_string psql_ch sql;
  flush psql_ch;
  ignore (Unix.close_process_out psql_ch)
