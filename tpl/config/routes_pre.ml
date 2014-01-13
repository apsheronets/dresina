open Cd_All
open Strings.Latin1
open Printf

type binding =
  { arg_ident : string
      (* identifier with bound string (argument name to be passed to
         controller) *)
  ; bound_ident : string
      (* ml code with bound value (__uri_patt_N) *)
  ; from_string : string
      (* ml code that translates __uri_patt to ml value *)
  ; to_string : string -> string
      (* fun expr_ml_code -> code returning string to be glued into path/url
         for this component
       *)
  }

type action =
  { cntr_name : string
  ; action_name : string
  ; bindings : binding list
  ; linedir : string
  }

type level =
| Bind of string (* ident *) * level (* below *)
| Map of (string * level) list
| Action of action

type context = level ref

(****************************)

let no_route_ml = "raise No_route"

let binding ~level ~ty ~id =
  let b = Expr.lid ("__uri_patt_" ^ string_of_int level) in
  let (from_string, to_string) =
    match ty with
    | "int" ->
        ( "(try int_of_string " ^ b ^ " with Failure _ -> " ^ no_route_ml ^ ")"
        , fun ident -> "string_of_int " ^ ident
        )
    | "string" ->
        ( b
        , fun ident -> ident
        )
    | _ -> failwith
        "routes: type %S is not supported in uri pattern" ty
  in
  { arg_ident = id
  ; bound_ident = b
  ; from_string = from_string
  ; to_string = to_string
  }

let meta_segs path =
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

let rec add_handler level mpath action =
  match mpath, level with
  | [], (Bind _ | Map _) -> failwith
      "routes: uri pattern is too short"
  | [], Action _ -> failwith
      "routes: route already exists"
  | (`Fixed _ :: _t), Bind _ -> failwith
      "routes: uri pattern contains fixed segment while other routes already \
       bind it as variable"
  | (`Binding _ :: _t), Map _ -> failwith
      "routes: uri pattern contains binding while other routes already use \
       this uri segment as fixed one"
  | ((`Fixed _ | `Binding _) :: _t), Action _ -> failwith
      "routes: a shorter route already matches this pattern"
  | `Binding _ :: t, Bind (_ident, level) ->
      add_handler level t action
  | (`Fixed s :: t), Map map ->
      Map
        (match List.Assoc.get_opt ~keq s map with
         | None ->
             let level =
               let rec loop mpath =
                 match mpath with
                 | [] -> Action action
                 | `Fixed s :: t ->
                     Map [(s, loop t)]
                 | `Binding b :: t ->
                     Bind (b.bound_ident, loop t)
               in
                 loop t
             in
               List.Assoc.add s level map
         | Some level ->
             List.Assoc.replace ~keq s (add_handler level t action) map
        )

(**************************************)

let get3 uri_patt_str cntr_name cntr_meth context =
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
          { cntr_name = uid ~place:"Controller name" cntr_name
          ; action_name = Expr.lid cntr_meth
          ; bindings = bindings
          ; linedir = directive_linedir ()
          }
        in
        context := add_handler !context mpath action
          
