let rg_dir = line_directive "_routing_generated_" 0

let conctx_modty = "Proj_common.CONTROLLER_CONTEXT"

let routing_action a =
  Expr.let_in "module Ctx" ("(val conctx : " ^ conctx_modty ^ ")") begin
  "\n" ^ a.linedir ^
  Expr.let_in ~oneline:true (sprintf "module %s" a.cntr_name)
    (sprintf "%s.Controller(Ctx)" a.cntr_name)
  begin
    "\n" ^ a.linedir ^
    "((" ^ Expr.modqual a.cntr_name a.action_name ^
    String.concat "" begin
        List.map
          (fun b ->
             Printf.sprintf " ~%s:(%s)" b.arg_ident b.from_string
          )
          a.bindings
      end
    ^ ") : unit -> Amall_http.response IO.m)\n" ^ rg_dir
  end
  end


let rec generate_routing_level level routes =
  if routes = []
  then
    no_route_ml
  else
    let (this_level, next_levels) = List.partition
      (fun (mp, _ac) -> mp = [])
      routes
    in
    Expr.match_ "path"
      [ ( "[] | \"\" :: []"
        , match this_level with
          | [] -> no_route_ml
          | (mp, ac) :: _t ->
              assert (mp = []);
              (* todo:
              List.iter
                (fun rt ->
                   ... "Unused route."
                )
                t;
              *)
              routing_action ac
        )
      ;
        if next_levels = []
        then
          ( "_"
          , no_route_ml
          )
        else
          let this_seg_ident = bound_ident ~level in
          ( this_seg_ident ^ " :: path"
          , let (fixeds, bindings) = List.partition_map
              (fun (mp, ac) ->
                 match mp with
                 | [] -> assert false
                 | fst :: rest ->
                     match fst with
                     | `Fixed str -> `Left (str, (rest, ac))
                     | `Binding _ -> `Right (rest, ac)
              )
              next_levels
            in
            let fixeds_groupped = List.group_pairs
              ~fst_eq:String.eq fixeds in
            Expr.match_ this_seg_ident
              ( List.map
                  (fun (grp, routes) ->
                     ( Lit.string grp
                     , generate_routing_level (level + 1) routes
                     )
                  )
                  fixeds_groupped
              @
                [ ( "_"
                  , if bindings = []
                    then no_route_ml
                    else generate_routing_level (level + 1) bindings
                  )
                ]
              )
          )
      ]


let generate_routing routes =
  rg_dir ^
  "open Main_pre\n" ^
  "open Proj_common\n" ^
  let by_meth = List.group_pairs ~fst_eq:( = ) routes in
  let meth_no_routes =
    List.minus
      all_meths ~proj1:identity
      by_meth ~proj2:fst
      ~compare:Pervasives.compare
  in
  Struc.func "routes" ["meth"; "path"; "conctx"] &
  Expr.match_ "meth" begin
    List.map
      (fun (meth, level) ->
         ( ml_of_meth meth
         , (generate_routing_level 0 level)
         )
      )
      by_meth
    @
    List.map (fun m -> (ml_of_meth m, no_route_ml)) meth_no_routes
  end


type con_seg_bind =
[ `Cseg of string
| `Cbind of int
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
     | `Cbind level ->
         match List.filter (fun b -> b.level = level) bs with
         | [b] -> `Ccode (b.arg_ident, b.to_string)
         | _ -> assert false

let gather_controllers routes =
  List.map
    (fun (mp, ac) ->
       let path = List.mapi
         (fun level ->
          function
          | `Fixed s -> `Cseg s
          | `Binding _b -> `Cbind level
         )
         mp
       in
       let segcode = List.map (cscode_of_csbind ac.bindings) path in
       let strcode = add_slashes segcode in
       let csc = concat_str_code strcode in
       (ac, csc)
    )
    routes


let generate_routes routes =
  line_directive "_routes_generated_" 0 ^ begin
  routes
  |> List.map snd  (* remove request methods *)
  |> gather_controllers
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

let ctx = ref []

let generate mlt =
  begin
    List.iter
      (function
       | Ml txt -> out_routing txt
       | Dir f -> f ctx
      )
      mlt
  end;
  let r = List.rev !ctx in
  out_routing (generate_routing r);
  out_routes (generate_routes r)
