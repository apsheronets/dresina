open Cd_All
open Strings.Latin1
open Printf
open Migrate_types
open Apply_migrations

let string_of_exn e =
  match e with
  | Dbi.Edbi e -> Dbi_pg.string_of_dbi_error e
  | _ -> Printexc.to_string e

let do_ema id conn ema =
  try
    begin match ema with
    | Ema_sql sql ->
        Dbi_pg.cmd_ok (conn#execute sql)
    | Ema_ocaml func_no ->
        let f = Migrations.get_ocaml_function id func_no in
        f conn
    end;
    None
  with
    e -> Some e

let dump_ema id = function
| Ema_sql sql -> "SQL: " ^
    (String.trim (function ' ' | '\n' | '\t' -> true | _ -> false) sql)
| Ema_ocaml func_no -> Printf.sprintf
    "OCaml function number %i from migration %S" func_no id

let do_migration id em : unit =
  Database.with_connection_blocking & fun conn ->
  let rollback done_rev =
    List.iter
      (fun ema ->
         Printf.printf "Rolling back action: %s\n%!" (dump_ema id ema);
         match do_ema id conn ema with
         | None -> ()
         | Some exn ->
             Printf.eprintf "Error rolling back action: %s\n%!"
               (string_of_exn exn)
      )
      done_rev;
    exit 1
  in
  let rec loop done_rev em =
    match em with
    | [] ->
        begin try
          Dbi_pg.cmd_ok &
          conn#execute_p "insert into schema_migrations (id) values ($1)"
            [| `String id |]
        with e ->
          Printf.eprintf
            "Error inserting migration id into schema_migrations: %s\n%!"
            (string_of_exn e)
        end
    | emi :: em ->
        let (ema_opt, done_rev') =
          match emi with
          | Emi_up ema_up -> Some ema_up, done_rev
          | Emi_down ema_down -> None, ema_down :: done_rev
          | Emi_up_down (ema_up, ema_down) -> Some ema_up, ema_down :: done_rev
        in
        let res_opt =
          match ema_opt with
          | None -> None
          | Some ema ->
              Printf.printf "Action: %s\n%!" (dump_ema id ema);
              do_ema id conn ema
        in
        match res_opt with
        | None -> loop done_rev' em
        | Some exn ->
            Printf.eprintf "Error applying migration action: %s\n\
                            Trying to roll back %i actions done.\n%!"
              (string_of_exn exn)
              (List.length done_rev);
            rollback done_rev
  in
    loop [] em

(* one of ids may be None, but not both.  None counts as "greater than
   any Some _".
 *)
let cmp_mig_id a b =
  match a, b with
  | None, None -> invalid_arg "cmp_mig_id"
  | Some _, None -> LT
  | None, Some _ -> GT
  | Some va, Some vb -> String.cmp va vb

let any_migrations_done = ref false

let schema = Schema_types.create_empty_schema ()

let do_migration_if_needed ~db_ver ~up_to id mig_list =
  let (em : Migrate_types.executable_migration) =
    apply_mig_list Generate_sql.methods schema id mig_list in
  if id <= db_ver
  then ()  (* is already applied, skipping *)
  else
  match cmp_mig_id (Some id) up_to with
  | GT -> ()  (* id > up_to *)
  | LT | EQ ->
      printf "Applying migration %S...\n%!" id;
      do_migration id em;
      any_migrations_done := true

let db_migrate ?up_to () =
  let db_ver = Database.get_schema_version () in
  printf "Database schema version = %S\n" db_ver;
  printf "Applying migrations%s.\n%!"
    (match up_to with None -> "" | Some v -> sprintf " up to id = %S" v);
  Register_all_migrations.register ();
  let m = Migrations.get () in
  Migrations.M.iter (do_migration_if_needed ~db_ver ~up_to) m;
  if not !any_migrations_done
  then printf "No migrations to apply.\n%!"
  else ()
