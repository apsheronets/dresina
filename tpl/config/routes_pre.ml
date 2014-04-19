open Cd_All
open Strings.Latin1
open Printf

type binding =
  { arg_ident : string
      (* identifier with bound string (argument name to be passed to
         controller) *)
  ; level : int
  ; from_string : expr
      (* expression to convert bound "__uri_patt" (.arg_ident) to typed value.
         returns "no_route_ml" in case of error. *)
  ; to_string : expr -> expr
      (* "fun expr_ml_code -> code" returning string to be glued into path/url
         for this component
       *)
  }

let bound_ident ~level = "__uri_patt_" ^ string_of_int level

type action =
  { cntr_name : string
  ; action_name : string
  ; bindings : binding list
  ; loc : string * int
  }

type metaseg =
[ `Fixed of string
| `Binding of binding
]

type meth = [ `GET | `POST ]
let all_meths = [`GET; `POST]
let patt_of_meth : meth -> patt = function
| `GET  -> Patt.poly "`GET" []
| `POST -> Patt.poly "`POST" []

type metapath = metaseg list

type route = meth * (metapath * action)

type context = route list ref

(****************************)


let no_route_expr = Expr.call "raise" [Expr.constr "No_route" []]

let binding ~level ~ty ~id =
  let b = bound_ident ~level in
  let (from_string, to_string) =
    let catch_failure body =
      Expr.try_ body
      [   Patt.constr "Failure" [Patt.any]
        , no_route_expr
      ]
    in
    match ty with
    | "int" ->
        ( catch_failure &
          Expr.call "int_of_string" [Expr.lid b]
        , fun x -> Expr.call "string_of_int" [x]
        )
    | "id" | "int64" ->
        ( catch_failure &
          Expr.call_mod ["Int64"] "of_string" [Expr.lid b]
        , fun x -> Expr.call_mod ["Int64"] "to_string" [x]
        )
    | "string" ->
        ( Expr.lid b
        , fun x -> x
        )
    | _ -> failwith
        "routes: type %S is not supported in uri pattern" ty
  in
  { arg_ident = id
  ; level = level
  ; from_string = from_string
  ; to_string = to_string
  }

let meta_segs path : metapath =
  List.mapi
    (fun level seg ->
       match String.split ( ( = ) ':' ) seg with
       | [ s ] -> `Fixed s
       | [ id ; ty ] ->
           `Binding (binding ~id ~level ~ty)
       | _ -> failwith "routes: bad segment: %S" seg
    )
    path

let keq = String.eq

let rec bindings_of_mpath ?(acc=[]) ?(level=0) mpath =
  match mpath with
  | [] -> List.rev_map snd acc
  | h :: t ->
      match h with
      | `Fixed _ -> bindings_of_mpath ~acc ~level:(level + 1) t
      | `Binding b ->
          if List.Assoc.mem ~keq b.arg_ident acc
          then failwith
            "routes: binding %S appears more than one time in uri pattern"
            b.arg_ident
          else
            let acc = List.Assoc.add b.arg_ident b acc
            in
            bindings_of_mpath ~acc ~level:(level + 1) t

let rec add_handler routes meth mpath action =
  (meth, (mpath, action)) :: routes

(**************************************)

let handler meth uri_patt_str cntr_name cntr_meth context =
  match Uri.parse uri_patt_str with
  | None -> failwith "routes: can't parse uri pattern"
  | Some uri_patt ->
      let open Uri_type in
      if uri_patt.scheme <> None
      then failwith "routes: expected no scheme in uri pattern"
      else if uri_patt.authority <> None
      then failwith "routes: expected no authority in uri pattern"
      else if uri_patt.query <> None
      then failwith "routes: '?query' is not supported now in uri pattern"
      else if uri_patt.fragment <> None
      then failwith "routes: '#fragment' won't work as one can expect"
      else
        let path = String.split ( ( = ) '/' ) uri_patt.path in
        let mpath = meta_segs path in
        let bindings = bindings_of_mpath mpath in
        let cntr_name = String.capitalize cntr_name in
        let action =
          { cntr_name =
             ( check_uid ~place:"Controller name" cntr_name; cntr_name )
          ; action_name =
             ( check_lid ~place:"Action name" cntr_meth; cntr_meth )
          ; bindings = bindings
          ; loc = directive_loc ()
          }
        in
        context := add_handler !context meth mpath action
          
open Mlt
let get3 = string_args3 & handler `GET
let post3 = string_args3 & handler `POST
