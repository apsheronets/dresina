open Cd_All
open Strings.Latin1
open Printf
open Migrate_types
open Apply_migrations

open Command_db_migrate

let rollback_done = ref false

let schema = Schema_types.create_empty_schema ()

let do_rollback id em =
  Database.with_connection_blocking & fun conn ->
  let rollback_exit () =
    Printf.eprintf
      "Rollback error.\n\
       NOTE: database is probably in inconsistent state.\n%!";
    exit 1
  in
  let execute_ema_list ema_list =
    let any_error = ref false in
    List.iter
      (fun ema ->
         Printf.printf "Rolling back: %s\n%!" (dump_ema id ema);
         match do_ema id conn ema with
         | None -> ()
         | Some exn ->
             Printf.eprintf "Error rolling back: %s\n%!"
               (string_of_exn exn);
             any_error := true
      )
      ema_list;
    begin try
      Dbi_pg.cmd_ok &
      conn#execute_p "delete from schema_migrations where id = $1"
        [| `String id |]
    with e ->
      Printf.eprintf
        "Error deleting migration id from schema_migrations: %s\n%!"
        (string_of_exn e);
      rollback_exit ()
    end;
    if !any_error
    then rollback_exit ()
    else ()
  in
  let rec prepare_ema_list to_roll_back em =
    match em with
    | [] -> to_roll_back
    | emi :: em ->
        match emi with
        | Emi_up _ema_up -> prepare_ema_list to_roll_back em
        | Emi_down ema_down
        | Emi_up_down (_, ema_down) ->
            prepare_ema_list (ema_down :: to_roll_back) em
  in
    let lst = prepare_ema_list [] em in
    execute_ema_list lst

let try_rollback ~rollback_id id mig_list =
  let (em : Migrate_types.executable_migration) =
    apply_mig_list Generate_sql.methods schema id mig_list in
  if id <> rollback_id
  then ()
  else begin
    do_rollback id em;
    rollback_done := true
  end

let db_rollback () =
  let db_ver = Database.get_schema_version () in
  printf "Database schema version = %S\n" db_ver;
  if db_ver <= "0"
  then begin
    Printf.eprintf "Can't rollback migration \"0\"\n%!";
    exit 1
  end else
  Register_all_migrations.register ();
  let m = Migrations.get () in
  Migrations.M.iter (try_rollback ~rollback_id:db_ver) m;
  if not !rollback_done
  then begin
    Printf.eprintf "Migration %S is not known to this executable.\n%!"
      db_ver;
    exit 1
  end else ()
