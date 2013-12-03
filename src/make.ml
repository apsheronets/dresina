open Common
open Cd_All

(*
let mdbg fmt = Printf.(ksprintf (fun s -> eprintf "MK: %s\n%!" s) fmt)
*)
let mdbg fmt = Printf.ifprintf stdout fmt

let no_digest = "\x00"

let digests_fn = "proj-build/.build_digests"

(* fname -> (stored_digest, new_digest).
   digest can be (==)no_digest when not present
 *)
let digests = lazy begin
  let digests = Hashtbl.create 17 in
  let () = begin
    Hashtbl.clear digests;
    if Sys.file_exists digests_fn
    then
      Filew.with_file_in_bin digests_fn begin fun ch ->
        let arr = Marshal.from_channel ch in
        Array.iter
          (fun (fn, dig) ->
             mdbg "loading digest of %S" fn;
             Hashtbl.replace digests fn (dig, no_digest)
          )
          arr
      end
    else
      ()
  end
  in
    digests
end


let save_digests () =
  Filew.with_file_out_bin digests_fn begin fun ch ->
    let lst =
      Hashtbl.fold
        (fun fn (_old_dig, new_dig) acc ->
           if new_dig == no_digest
           then acc
           else (fn, new_dig) :: acc
        )
        !!digests
        []
    in
    let arr = Array.of_list lst in
    Marshal.(to_channel ch arr [No_sharing])
  end


(********************************)

let rec create_dirs_for_file fn =
  let dir = Filename.dirname fn in
  if Sys.file_exists dir
  then begin
    if Sys.is_directory dir
    then ()
    else failwith "%S expected to be directory, but it is a file" dir
  end else begin
    create_dirs_for_file dir;
    mdbg "creating directory %S" dir;
    Unix.mkdir dir 0o777
  end


type modified =
| M_not_modified
| M_rebuilt

type status =
| Actual of modified
| Not_checked

type rule =
  { deps : string list
  ; build_func : unit -> unit
  ; status : status ref
  ; create_dirs : unit -> unit
  }

let rules = Hashtbl.create 17

let make targets deps func =
  let status = ref Not_checked in
  let create_dirs () =
    List.iter create_dirs_for_file targets
  in
  List.iter
    begin fun target ->
      if Hashtbl.mem rules target
      then failwith "Make: rule for %S already exists" target
      else
        mdbg "Make: registering target %S with dependencies [%s]"
          target (String.concat "; " & List.map (Printf.sprintf "%S") deps);
        Hashtbl.add
          rules
          target
          { deps = deps
          ; build_func = func
          ; status = status
          ; create_dirs = create_dirs
          }
    end
    targets

let make1 target deps func = make [target] deps func

(**)

let hashtbl_find_opt h k = try Some (Hashtbl.find h k) with Not_found -> None

let does_digest_match fn =
  match hashtbl_find_opt !!digests fn with
  | None ->
      let current_digest = Digest.file fn in
      Hashtbl.add !!digests fn (no_digest, current_digest);
      mdbg "does_digest_match %S: no stored digest, doesn't match" fn;
      false
  | Some (old_dig, new_dig) ->
      let new_dig =
        if new_dig == no_digest
        then begin
          mdbg "does_digest_match %S:   no new digest" fn;
          let current_digest = Digest.file fn in
          Hashtbl.replace !!digests fn (old_dig, current_digest);
          current_digest
        end else begin
          mdbg "does_digest_match %S:   has new digest" fn;
          new_dig
        end
      in
        let r = (old_dig = new_dig) in
        mdbg "does_digest_match %S: %b" fn r;
        r

type rebuild =
| Rebuild_needed
| No_rebuild

let rec build target =
  match hashtbl_find_opt rules target with
  | None ->
      let matches = does_digest_match target in
      mdbg "building %S, it's not a target, digest matches? = %b"
        target matches;
      if matches
      then No_rebuild
      else Rebuild_needed
  | Some r ->
      begin
        mdbg "building %S, it's a target, actual? = %b" target
          (!(r.status) <> Not_checked);
        match !(r.status) with
        | Actual M_not_modified -> No_rebuild
        | Actual M_rebuilt -> Rebuild_needed
        | Not_checked ->
            begin
              let deps_rebuild =
                (* !  we need to run "build dep" on _all_ deps to get all
                   files' digests, to store them later in .build_digests!
                   So here are no List.exists, and no "acc || build dep..".
                 *)
                List.fold_left
                  (fun acc dep -> (build dep = Rebuild_needed) || acc)
                  false
                  r.deps
              in
              let rebuild_needed =
                   not (Sys.file_exists target)
                || deps_rebuild
              in
              mdbg "building %S, rebuild_needed? = %b" target rebuild_needed;
              begin
                if rebuild_needed
                then begin
                  r.create_dirs ();
                  begin
                    try
                      r.build_func ()
                    with
                    | e -> failwith "Make: failed.  Target: %s, deps: %s, \
                                     exception: %s"
                        target (String.concat ", " r.deps)
                        (Printexc.to_string e)
                  end;
                  r.status := Actual M_rebuilt
                end else begin
                  r.status := Actual M_not_modified
                end
              end;
              if rebuild_needed
              then Rebuild_needed
              else No_rebuild
            end
      end

let do_make () =
  ( Hashtbl.iter
      (fun target _rule -> ignore (build target))
      rules
  ; Hashtbl.iter (fun _target r -> assert (!(r.status) <> Not_checked)) rules
  ; save_digests ()
  )

let do_clean () =
  Hashtbl.iter
    (fun target _rule -> Sys.remove target
    )
    rules;
  Sys.remove digests_fn
