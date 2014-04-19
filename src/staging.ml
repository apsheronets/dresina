
(*

в качестве ещё одного куска проверок -- type context' = context после pre,
чтобы диагностировать отсутствие определения context в pre.

*)



(* Данный модуль обеспечивает генерацию .ml-файлов на основании .mlt-файла
   (ml + templates -- ml-подобного файла со вкраплениями чужеродного
   синтаксиса) и помогающих генерации .ml-файлов (в том числе с обработчиками
   чужеродностей).

   .mlt состоит из обычного кода на окамле и из специальных кусков,
   обрабатываемых особым образом.

   Куски имеют синтаксис

     % func arg1 arg2 .. argN

   либо

     % begin func arg1 arg2 .. argN
        <body>
     % end

   и весьма вольную семантику.
   Предполагается, что в помогающих генерации .ml-файлах есть функция funcN,
   берущая N аргументов-строк, содержащих arg1 .. argN.  В случае наличия
   тела вызывается funcNb, берущая N аргументов и аргумент-тело (он
   содержит в себе line-directive).

   (очевидно, надо поддерживать не только строки, но и более хитрое --
   списки, хеши; тело в случае % begin func ... \n <body> \n % end;
   в этих случаях имя функции будет другим, например, func4_hash2_list3_body
   для функции с arg2:hash, arg3:list, с 4 аргументами и принимающей тело).

   Далее всё говно из .mlt преобразуется в список
   [ Ml of string | Dir of (context -> unit) ]
   (context должен быть определён где-то в модулях-помощниках pre-серии;
   содержимое Dir является ответом от funcN.)

   Места в mlt, где был встречен код, хранимый в:
     - Ml -- вписаны прямо в string, в виде первой строки с line-директивой
     - Dir -- присутствуют в определении списка [ Ml | Dir ], то есть, в случае
       ошибок ("нет функции для директивы", типично) компилятор показывает
       место, где была встречена директива.

   И конечная функция
     generate :
       [ Ml | Dir ] list ->
       unit
   позволяет срать в генерируемый файл вызовом предопределённой функции
   out : string -> unit.

   Порядок помощников до и после .mlt задаётся явно, это списки .ml-файлов.

   Далее всё это склеивается в один .ml-файл, компилируется в байткод и
   запускается.  (в один .ml -- внимание насчёт переопределений!)

   Во всех файлах (pre, post) доступно значение __mlt_filename с именем
   обрабатываемого .mlt.
*)

module Cg = Codegen

open Cd_All
open Strings.Latin1
open Printf
open Common


let is_space c = (c == '\x20' || c == '\x09')


let datadef_of_mlt fname =
  Filew.with_file_in_bin fname & fun ch ->
    dbg "mll %S" fname;
    let lexbuf = Lexing.from_channel ch in
    Mlt_parser.file [] fname 1 lexbuf

open Ml_comp

let mlt_typedefs_si =
  let open Cg in
  let open Typ in
  [ Struc.linedir "_mlt_typedefs_" 0
  ; Struc.type_
      (prim "mldir")
      (sum
         [ ("Ml", [prim "string"]);
           ("Dir", [arrow [ prim "context" ; prim "unit" ]])
         ]
      )
  ]
let mlt_typedefs = Cg.Implem.to_string mlt_typedefs_si

let output_mlt ~out_ch list_item_codes : unit =
  output_string out_ch & Cg.Implem.to_string & [Cg.Struc.linedir "_mlt_" 0];
  output_string out_ch & Cg.Implem.to_string & List.one &
  Cg.Struc.expr ~typ:(Cg.Typ.(param (prim "list") [prim "mldir"]))
    "mlt"
    (Cg.Expr.list list_item_codes)

let out_ch_ident ~i = sprintf "out_ch_%i" i



let prepare_output outs = Cg.Implem.to_string
( Cg.Struc.linedir "_prepare_output_" 0
::
  begin
    List.flatten &
    List.mapi begin fun i (out_suffix, _out_fname) ->
      let i = i + 1 in
      let out_ident =
        if out_suffix = ""
        then "out"
        else "out_" ^ out_suffix in
      let out_ident_raw = out_ident ^ "_raw" in
      let ch : string = out_ch_ident ~i in
      let open Cg in
      [ Struc.expr
          ch
          (Expr.call "open_out_bin"
             [Expr.inj & sprintf "Sys.argv.(%i)" i]
          )
      ; Struc.expr
          out_ident_raw
          (Expr.call "output_string" [Expr.lid ch])
      ; Struc.func
          out_ident
          [Patt.lid "struc_items"]
          (Expr.call out_ident_raw
             [Expr.call_mod ["Implem"] "to_string" [Expr.lid "struc_items"]
             ]
          )
      ]
    end
    outs
  end
)

let call_generation outs =
  let open Cg in
  let open Expr in
  let close_all_expr =
    seq & List.mapi begin fun i _ ->
        let i = i + 1 in
        Cg.Expr.(call "close_out_noerr" [lid & out_ch_ident ~i])
      end
      outs
  in
  let remove_all_expr =
    for_ "i" (int 1) (int (List.length outs)) begin
      let_in (Patt.lid "fn") (inj "Sys.argv.(i)") begin
        try_ begin
          call_mod ["Sys"] "remove" [lid "fn"]
        end
        [ ( Patt.lid "e"
          , call_mod ["Printf"] "eprintf"
             [ string "warning: can't remove file %s: %s"
             ; lid "fn"
             ; call_mod ["Printexc"] "to_string" [lid "e"]
             ]
          )
        ]
      end
    end
  in
  Implem.to_string &
  [ Struc.linedir "_call_generation_" 0
  ; Struc.let_ Patt.unit &
    let open Expr in
    let_in Patt.(func "close_all" [unit]) close_all_expr &
    let_in Patt.(func "remove_all" [unit]) remove_all_expr &
    try_ begin seq
      [ call "generate" [lid "mlt"]
      ; call "close_all" [unit]
      ]
      end
      [ ( Patt.lid "e"
        , seq
            [ call "close_all" [unit]
            ; call "remove_all" [unit]
            ; call "raise" [lid "e"]
            ]
        )
      ]
  ]

let dir_with_loc =
"let msg_of_exn e = match e with \n\
 | (Failure msg | Invalid_argument msg) -> msg \n\
 | e -> Printexc.to_string e \n\
 ;; \n\
 let __dir_fname = ref \"\" and __dir_lineno = ref 0;; \n\
 let directive_linedir () = line_directive !__dir_fname !__dir_lineno;; \n\
 let directive_loc () = (!__dir_fname, !__dir_lineno);; \n\
 let dir_with_loc fname lineno dir ctx : unit = \n\
  __dir_fname := fname; __dir_lineno := lineno; \n\
   try dir ctx with e -> \n\
     ( output_codegen_error fname lineno (msg_of_exn e) \n\
     ; if Printexc.backtrace_status () then Printexc.print_backtrace stderr\n\
     ; exit 1 \n\
     ) \n\
 ;; \n\
 let make_body bodycode = (!__dir_fname, !__dir_lineno, bodycode);;\n\
 let dir_struc_linedir () = Struc.linedir !__dir_fname !__dir_lineno;;\n\
"

let codegen_module = "proj-build/internal/common/codegen.ml.module"

(* outs: list of ("ident_for_output_command", "output_to_filename.ml") *)
let do_stage_multi ?(pkgs = []) ~mlt ~pre ~post ~outs () =
  assert (outs <> []);
  let has_dups_by proj =
    List.length outs
      <> List.length (List.uniq ~eq:(fun a b -> proj a = proj b) outs) in
  if has_dups_by fst
  then invalid_arg "staging: duplicate output identifiers"
  else if has_dups_by snd
  then invalid_arg "staging: duplicate output file names"
  else
  let pkgs = "cadastr" :: "pprint" :: pkgs in
  let (tmpfn, out_ch) = Filename.open_temp_file
    ~mode:[Open_binary] "stage" ".ml" in
  copy_ml_to_channel ~out_ch codegen_module;
  output_string out_ch
    "open Codegen;; open Cg2;; module Mlt = Codegen.Cg1.Mlt;;\n";
  output_string out_ch (prepare_output outs);
  output_string out_ch dir_with_loc;
  output_string out_ch & Cg.Implem.to_string [Cg.Struc.linedir "_staging_" 0];
  output_string out_ch &
    Cg.Implem.to_string &
    [ Cg.Struc.expr "__mlt_filename" & Cg.Expr.string mlt ];
  copy_mls_to_channel ~out_ch ~files:pre;
  output_string out_ch mlt_typedefs;
  output_mlt ~out_ch (datadef_of_mlt mlt (* 1 (Filew.file_lines mlt) *) );
  copy_mls_to_channel ~out_ch ~files:post;
  output_string out_ch (call_generation outs);
  close_out out_ch;
  let tmpfn_prefix = Filename.chop_suffix tmpfn ".ml" in
  let byt = tmpfn_prefix ^ ".byte" in
  let errc = compile_ml_to_byt ~pkgs tmpfn byt in
  if errc <> 0
  then failwith "stage: compilation error; source left in %s" tmpfn
  else
  let errc =
    sys_command & sprintf "ocamlrun %s %s"
      (Filename.quote byt)
      (String.concat " " & List.map (snd @> Filename.quote) outs)
  in
  if errc <> 0
  then
    failwith "stage: runtime error; source left in %s, bytecode in %s"
      tmpfn byt
  else
    List.iter Sys.remove
      [ tmpfn
      ; tmpfn_prefix ^ ".cmi"
      ; tmpfn_prefix ^ ".cmo"
      ; byt
      ]

let do_stage ?(pkgs = []) ~mlt ~pre ~post ~fname =
  do_stage_multi ~pkgs ~mlt ~pre ~post ~outs:[("", fname)]

let stage_multi ?(pkgs = []) ?(add_deps = []) ~mlt ~pre ~post targets =
  Make.make
    (List.map snd targets)
    (codegen_module :: mlt :: 
       List.rev_append add_deps (List.rev_append pre post)
    ) &
    do_stage_multi ~pkgs ~mlt ~pre ~post ~outs:targets

let stage ?(pkgs = []) ?(add_deps = []) ~mlt ~pre ~post target =
  stage_multi ~pkgs ~add_deps ~mlt ~pre ~post [("", target)]

let has_slash p = String.contains p '/'

(* calls [stage] with common conventions on files paths *)
let stage_multi_paths ?(pkgs = []) ~rel_path ~mlt ~pre ~post targets =
  let mlt = "proj" // rel_path // mlt
  and (pre, post) = Tuple2.map_mono
    (List.map
       (fun ml ->
          if has_slash ml
          then ml
          else "tpl" // rel_path // ml
       )
    )
    (pre, post)
  and targets =
    List.map
      (fun (output_suffix, target) ->
         ( output_suffix
         , if has_slash target
           then target
           else "proj-build" // rel_path // target
         )
      )
      targets
  in
  stage_multi ~pkgs ~mlt ~pre ~post targets

let stage_paths ?(pkgs = []) ~rel_path ~mlt ~pre ~post target =
  stage_multi_paths ~pkgs ~rel_path ~mlt ~pre ~post
    [("", target)]
