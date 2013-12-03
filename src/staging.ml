
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

let prepare_output =
  Cg.line_directive "_prepare_output_" 0 ^
  "let out_fn = Sys.argv.(1);;\n\
   let out_ch = open_out_bin out_fn;;\n\
   let out = output_string out_ch;;\n"

let call_generation =
  Cg.line_directive "_call_generation_" 0 ^
  "let () = \n\
     try \n\
       generate mlt; \n\
       close_out out_ch \n\
     with \n\
     | e -> \n\
         close_out out_ch; \n\
         Sys.remove out_fn; \n\
         raise e \n\
  "

let do_stage ?(pkgs = []) ~mlt ~pre ~post ~fname () =
  let pkgs = "cadastr" :: pkgs in
  let (tmpfn, out_ch) = Filename.open_temp_file
    ~mode:[Open_binary] "stage" ".ml" in
  let pre = "src/codegen.ml" :: pre in
  output_string out_ch prepare_output;
  output_string out_ch Cg.dummy_line_directive;
  output_string out_ch & Cg.Struc.expr "__mlt_filename" & Cg.Lit.string mlt;
  copy_mls_to_channel ~out_ch ~files:pre;
  output_string out_ch mlt_typedefs;
  output_mlt ~out_ch (datadef_of_mlt mlt (* 1 (Filew.file_lines mlt) *) );
  copy_mls_to_channel ~out_ch ~files:post;
  output_string out_ch call_generation;
  close_out out_ch;
  let byt = Filename.chop_suffix tmpfn ".ml" ^ ".byte" in
  let errc = compile_ml_to_byt ~pkgs tmpfn byt in
  if errc <> 0
  then failwith "stage: compilation error; source left in %s" tmpfn
  else
  let errc =
    sys_command & sprintf "ocamlrun %s %s"
      (Filename.quote byt) (Filename.quote fname)
  in
  if errc <> 0
  then
    failwith "stage: runtime error; source left in %s, bytecode in %s"
      tmpfn byt
  else
    Sys.remove tmpfn;
    Sys.remove byt;
    ()


let stage ?(pkgs = []) ~mlt ~pre ~post target =
  Make.make1 target (mlt :: (pre @ post)) &
  do_stage ~pkgs ~mlt ~pre ~post ~fname:target

let has_slash p =
  Cg.string_index_opt p '/' <> None

(* calls [stage] with common conventions on files paths *)
let stage_paths ?(pkgs = []) ~rel_path ~mlt ~pre ~post target =
  let ( / ) = Filename.concat in
  let mlt = "proj" / rel_path / mlt
  and (pre, post) = Tuple2.map_mono
    (List.map
       (fun ml ->
          if has_slash ml
          then ml
          else "tpl" / rel_path / ml
       )
    )
    (pre, post)
  and target = "proj-build" / rel_path / target in
  stage ~pkgs ~mlt ~pre ~post target
