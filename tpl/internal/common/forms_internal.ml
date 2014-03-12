open Cd_All
open Strings.Latin1
open Proj_common

let of_opt_exn place = function
| Some x -> x
| None -> failwith ("of_opt_exn at " ^ place)

exception Form_field_not_found of string

type model_validation_place =
| Mvp_model of string (* model *)
| Mvp_field of string (* model *)
             * string (* field *)

exception Model_validation of ((model_validation_place * exn) list)

let store_validation_error mvp errors e =
  let new_errors =
    match e with
    | Model_validation errs -> errs
    | err -> [(mvp, err)]
  in
  errors := !errors @ new_errors

let catch_to_res_mvp model_name f =
  catch
    (fun () -> f () >>= fun r -> return (`Ok r))
    (function
     | (Model_validation _) as e -> return (`Error e)
     | e -> return (`Error
         (Model_validation [(Mvp_model model_name, e)]))
    )

let add_to_paramsl paramsl k v =
  let params = !paramsl in
  paramsl := lazy (StrMap.add k v (Lazy.force params))

let dump_mvp = function
| Mvp_model m -> sprintf "Mvp_model %S" m
| Mvp_field (m, f) -> sprintf "Mvp_field (%S, %S)" m f

let form_add_error errors (mvp : model_validation_place) e =
  (* Printf.eprintf "form error: %s\n%!" (dump_mvp mvp); *)
  errors := (mvp, e) :: !errors;
  None

let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let trim_ws str =
  (* no need to trim with utf8-functions, since trimming 1-byte characters *)
  Strings.Latin1.String.trim is_whitespace str

(* returns:
   - None -- type error (it's stored in [errors])
   - Some None -- field not found ( => not updating model)
   - Some (Some x) -- ok
 *)
let update_from_form_field_or_store_error
  errors map paramsl (f : _ -> 'a) mdl fld
 :
  'a option option
 =
  let k = mdl ^ "." ^ fld in
  try
    Some begin match strmap_find_opt k map with
    | None -> None
    | Some str ->
        (* place for no-trim here *)
        let str = trim_ws str in
        add_to_paramsl paramsl k str;
        Some (f str)
    end
  with e -> form_add_error errors (Mvp_field (mdl, fld)) e

(* returns:
   - None -> type error (it's stored in [errors])
   - Some None -- field not found ( => not updating model)
   - Some (Some x) -- ok, where x : 'a option
 *)
let update_from_form_field_opt_or_store_error
  errors map paramsl (f : _ -> 'a) mdl fld
 :
  'a option option option
 =
  let k = mdl ^ "." ^ fld in
  try
    Some begin match strmap_find_opt k map with
    | None -> None
    | Some str ->
        (* place for no-trim here *)
        let str = trim_ws str in
        Some begin
          add_to_paramsl paramsl k str;
          if str = ""
          then None
          else Some (f str)
        end
    end
  with e -> form_add_error errors (Mvp_field (mdl, fld)) e

(* returns:
   - None -- type error or field not found (it's stored in [errors])
   - Some x -- ok
 *)
let form_field_or_store_error
  errors map paramsl (f : _ -> 'a) mdl fld
 :
  'a option
 =
  match
    update_from_form_field_or_store_error errors map paramsl f mdl fld
  with
  | None -> None
  | Some None ->
      let k = mdl ^ "." ^ fld in
      form_add_error errors (Mvp_field (mdl, fld)) (Form_field_not_found k)
  | Some ((Some _) as r) -> r

(* returns:
   - None -> type error (it's stored in [errors])
   - Some None -- field not found or is empty, attr value is [None]
   - Some (Some x) -- attr value is [Some x]
 *)
let form_field_opt_or_store_error
  errors map paramsl (f : _ -> 'a) mdl fld
 :
  'a option option
 =
  match
    update_from_form_field_opt_or_store_error errors map paramsl f mdl fld
  with
  | None -> None
  | Some None -> Some None
  | Some ((Some _) as x) -> x


let key_of_mvp place =
  let place =
    match place with
    | Mvp_model m -> m
    | Mvp_field (m, f) -> m ^ "." ^ f
  in
    place ^ ".errors"

let concat_errors a b =
  match a, b with
  | "", x | x, "" -> x
  | _ -> a ^ "\n" ^ b

let add_errors_to_params errors paramsl =
  let params = Lazy.force paramsl in
  List.fold_left
    (fun params (place, exn) ->
       let k = key_of_mvp place in
       let error = Printexc.to_string exn in
       let errors =
         match strmap_find_opt k params with
         | None -> error
         | Some e -> concat_errors e error
       in
       StrMap.add k errors params
    )
    params
    errors

let errors_of_exn_opt model_name exn_opt =
  match exn_opt with
  | None -> []
  | Some (Model_validation lst) -> lst
  | Some e -> [(Mvp_model model_name, e)]

let errors_of_mvp add_errors to_form_arg place =
  let error_from_params =
    match to_form_arg with
    | `New | `Instance _ -> ""
    | `Params p ->
        let k = key_of_mvp place in
        match strmap_find_opt k p with
        | None -> ""
        | Some e -> e
  and additional_error =
    match List.Assoc.get_opt ~keq:(=) place add_errors with
    | None -> ""
    | Some e -> Printexc.to_string e
  in
    concat_errors error_from_params additional_error

let mvp_of_model model_name place =
  match place with
  | Mvp_model m | Mvp_field (m, _) -> m = model_name

let all_model_errors add_errors to_form_arg model_name =
  let errors_from_params =
    match to_form_arg with
    | `New | `Instance _ -> ""
    | `Params p ->
        let model_name_dot = model_name ^ "." in
        StrMap.fold
          (fun k err acc ->
             if String.is_prefix ~string:k ~prefix:model_name_dot &&
                String.is_suffix ~string:k ~suffix:".errors"
             then
               let prefix = String.chop_suffix ~string:k ~suffix:".errors" in
               concat_errors acc (prefix ^ ": " ^ err)
             else
               acc
          )
          p
          ""
  and additional_errors =
    List.fold_left
      (fun acc (place, exn) ->
         if mvp_of_model model_name place
         then
           let prefix = key_of_mvp place in
           concat_errors acc (prefix ^ ": " ^ Printexc.to_string exn)
         else acc
      )
      ""
      add_errors
  in
    concat_errors errors_from_params additional_errors


let create_form_on_errors errors model_name actionl create_form paramsl =
  let create_form errors =
    let params = add_errors_to_params errors paramsl in
    `Form (create_form (`Params params))
  in
  if errors = []
  then begin
    try `Instance (Lazy.force actionl)
    with e ->
      begin match e with
      | Model_validation errors -> create_form errors
      | e -> create_form [(Mvp_model model_name, e)]
          (* unknown exception -> whole record error *)
      end
  end else begin
    create_form errors
  end

(* if [errors] is not empty, then creates form.
   if empty, then tries to create model instance, reverting to form when
   instance has errors (validation, for example).
 *)
let create_inst_or_form = create_form_on_errors

let update_or_create_form = create_form_on_errors
