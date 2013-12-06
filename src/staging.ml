
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

let mlt_typedefs =
  let open Cg in
  let open Typ in
    dummy_line_directive
  ^ Sum.typedef "mldir"
      [ ("Ml", [prim "string"]);
        ("Dir", [arrow [ prim "context" ; prim "unit" ]])
      ]

let output_mlt ~out_ch list_item_codes : unit =
  output_string out_ch Cg.dummy_line_directive;
  output_string out_ch &
  Cg.Struc.expr ~ty:(Cg.Typ.param "list" ["mldir"])
    "mlt"
    (Cg.Expr.list list_item_codes)

let out_ch_ident ~i =
  Cg.Expr.lid & sprintf "out_ch_%i" i

let prepare_output outs =
  Cg.line_directive "_prepare_output_" 0 ^
  String.concat "" begin
    List.mapi begin fun i (out_suffix, _out_fname) ->
        let i = i + 1 in
        let out_ident =
          if out_suffix = ""
          then "out"
          else Cg.Expr.lid ("out_" ^ out_suffix) in
        let ch = out_ch_ident ~i in
        Cg.Struc.expr
          ch
          (Cg.Expr.call "open_out_bin" [Cg.Arr.get "Sys.argv" & Cg.Lit.int i])
        ^
        Cg.Struc.expr
          out_ident
          (Cg.Expr.call "output_string" [ch])
      end
      outs
  end

let call_generation outs =
  let close_all_code =
    Cg.Expr.seq & List.mapi begin fun i _ ->
        let i = i + 1 in
        Cg.Expr.call "close_out_noerr" [out_ch_ident ~i]
      end
      outs
  in
  let remove_all_code =
    let i = Cg.Expr.lid "i" in
    Cg.Expr.for_ i (Cg.Lit.int 1) (Cg.Lit.int (List.length outs)) [
      Cg.Expr.let_in "fn" (Cg.Arr.get "Sys.argv" i) begin
        "try Sys.remove fn \n\
         with e -> \n\
           Printf.eprintf \"warning: can't remove file %s: %s\" \n\
             fn (Printexc.to_string e) \n\
        "
      end
    ]
  in
  Cg.line_directive "_call_generation_" 0 ^
    Cg.Struc.expr "()" begin
    Cg.Expr.let_in "close_all ()" close_all_code begin
    Cg.Expr.let_in "remove_all ()" remove_all_code begin
      "try \n\
         generate mlt; \n\
         close_all () \n\
       with \n\
       | e -> \n\
           close_all (); \n\
           remove_all (); \n\
           raise e \n\
      "
    end
    end
    end

let dir_with_loc =
"let msg_of_exn e = match e with \n\
 | (Failure msg | Invalid_argument msg) -> msg \n\
 | e -> Printexc.to_string e \n\
 ;; \n\
 let __dir_fname = ref \"\" and __dir_lineno = ref 0;; \n\
 let directive_linedir () = line_directive !__dir_fname !__dir_lineno;; \n\
 let dir_with_loc fname lineno dir ctx = \n\
  __dir_fname := fname; __dir_lineno := lineno; \n\
   try dir ctx with e -> \n\
     ( codegen_error fname lineno (msg_of_exn e) \n\
     ; exit 1 \n\
     ) \n\
 ;; \n\
"

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
  let pkgs = "cadastr" :: pkgs in
  let (tmpfn, out_ch) = Filename.open_temp_file
    ~mode:[Open_binary] "stage" ".ml" in
  output_string out_ch (prepare_output outs);
  copy_ml_to_channel ~out_ch "src/codegen.ml";
  output_string out_ch dir_with_loc;
  output_string out_ch Cg.dummy_line_directive;
  output_string out_ch & Cg.Struc.expr "__mlt_filename" & Cg.Lit.string mlt;
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

let stage_multi ?(pkgs = []) ~mlt ~pre ~post targets =
  Make.make (List.map snd targets) (mlt :: (pre @ post)) &
  do_stage_multi ~pkgs ~mlt ~pre ~post ~outs:targets

let stage ?(pkgs = []) ~mlt ~pre ~post target =
  stage_multi ~pkgs ~mlt ~pre ~post [("", target)]

let has_slash p =
  Cg.string_index_opt p '/' <> None

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
         (output_suffix, "proj-build" // rel_path // target)
      )
      targets
  in
  stage_multi ~pkgs ~mlt ~pre ~post targets

let stage_paths ?(pkgs = []) ~rel_path ~mlt ~pre ~post target =
  stage_multi_paths ~pkgs ~rel_path ~mlt ~pre ~post
    [("", target)]
