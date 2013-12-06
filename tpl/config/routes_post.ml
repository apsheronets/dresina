(*
let rec dump_routes ?(indent = 0) level =
  let ind = String.make (4 * indent) ' ' in
  let pr fmt = Printf.(ksprintf (fun s -> printf "%s%s\n%!" ind s) fmt) in
  match level with
  | Action a -> pr "action: %s" a
  | Bind (bind_ident, level) ->
      (pr "bind %s" bind_ident; dump_routes ~indent:(indent + 1) level)
  | Map m ->
      List.iter
        (fun (s, level) ->
           (pr "%S" s; dump_routes ~indent:(indent + 1) level)
        )
        m
*)

let rg_dir = line_directive "_routing_generated_" 0

let conctx_modty = "Proj_common.CONTROLLER_CONTEXT"

let routing_action a =
  Expr.let_in "module Ctx" ("(val conctx : " ^ conctx_modty ^ ")") begin
  "\n" ^ a.linedir ^
  Expr.let_in ~oneline:true "module Con"
    (sprintf "%s.Controller(Ctx)" a.cntr_name)
  begin
    "\n" ^ a.linedir ^
    "(((" ^ Expr.modqual "Con" a.action_name ^
    String.concat "" begin
        List.map
          (fun b ->
             Printf.sprintf " ~%s:(%s)" b.arg_ident b.from_string
          )
          a.bindings
      end
    ^ ") ()) : Proj_common.response)\n" ^ rg_dir
  end
  end

let rec generate_routing_level level =
  match level with
  | Action a ->
      Expr.match_ "path"
        [ ("[] | \"\" :: []", routing_action a)
        ; ("_", no_route_ml)
        ]
  | Bind (bind_ident, level) ->
      Expr.match_ "path"
        [ ("[]", no_route_ml)
        ; ( sprintf "%s :: path" bind_ident
          , generate_routing_level level
          )
        ]
  | Map m ->
      Expr.match_ "path"
        ( List.map
            (fun (s, level) ->
               ( sprintf "%s :: path" (Lit.string s)
               , generate_routing_level level
               )
            )
            m
        @ [("_", no_route_ml)]
        )

let generate_routing level =
  rg_dir ^
  Struc.func "routes" ["path"; "conctx"] (generate_routing_level level)

type con_seg_bind =
[ `Cseg of string
| `Cbind of string
]

type con_seg_code =
[ `Cseg of string
| `Ccode of (string (* arg_ident *) * (string -> string) (* to_string *) )
]

type con_str_code =
[ `Cstr of string
| `Ccode of (string (* arg_ident *) * (string -> string) (* to_string *) )
]

let add_slashes
 : con_seg_code list -> con_str_code list
 = fun lst ->
     List.flatten &
     List.map
       (let slash x = [ `Cstr "/" ; x ] in
        function
        | `Cseg s -> slash & `Cstr s
        | (`Ccode _) as c -> slash & c
       )
       lst

let rec concat_str_code
 : con_str_code list -> con_str_code list
 = fun csc ->
     match csc with
     | [] -> []
     | `Cstr s1 :: `Cstr s2 :: t ->
         concat_str_code (`Cstr (s1 ^ s2) :: t)
     | ((`Cstr _) as h) :: (((`Ccode _) :: _ | []) as t)
     | ((`Ccode _) as h) :: t ->
         h :: concat_str_code t

let cscode_of_csbind
 : binding list -> con_seg_bind -> con_seg_code
 = fun bs csb ->
     match csb with
     | (`Cseg _) as cseg -> cseg
     | `Cbind bound_ident ->
         match List.filter (fun b -> b.bound_ident = bound_ident) bs with
         | [b] -> `Ccode (b.arg_ident, b.to_string)
         | _ -> assert false

let rec gather_controllers ~acc ~path_rev level =
  match level with
  | Map m ->
      List.fold_left
        (fun acc (seg, level) ->
           let path_rev = `Cseg seg :: path_rev in
           gather_controllers ~acc ~path_rev level
        )
        acc
        m
  | Bind (bound_ident, level) ->
      gather_controllers ~acc ~path_rev:(`Cbind bound_ident :: path_rev) level
  | Action a ->
      let segcode = List.rev_map (cscode_of_csbind a.bindings) path_rev in
      let strcode = add_slashes segcode in
      let csc = concat_str_code strcode in
      (a, csc) :: acc

let generate_routes level =
  line_directive "_routes_generated_" 0 ^ begin
  level
  |> gather_controllers ~acc:[] ~path_rev:[]
  |> List.map
       (fun ((action, _csc) as x) ->
          (action.cntr_name, x)
       )
  |> List.group_pairs ~fst_eq:String.eq
  |> List.map
       (fun (cntr_name, submodules) ->
          Struc.module_ cntr_name &
            List.map
              (fun (a, csc) ->
                 Struc.module_ (String.capitalize a.action_name) &
                   [ sprintf "let path%s = %s"
                       (String.concat "" &
                        List.map_filter
                          (function
                           | `Cstr _ -> None
                           | `Ccode (arg_ident, _to_string) ->
                                Some (" ~" ^ arg_ident)
                          )
                          csc
                       )
                       (String.concat " ^ " &
                        List.map
                          (function
                           | `Cstr s -> Lit.string s
                           | `Ccode (arg_ident, to_string) ->
                               to_string arg_ident
                          )
                          csc
                       )
                   ]
              )
              submodules
       )
  |> Struc.items
  end

(******************************************)

let ctx = ref (Map [])

let generate mlt =
  begin
    List.iter
      (function
       | Ml txt -> out_routing txt
       | Dir f -> f ctx
      )
      mlt
  end;
  (* dump_routes !ctx *)
  out_routing (generate_routing !ctx);
  out_routes (generate_routes !ctx)
