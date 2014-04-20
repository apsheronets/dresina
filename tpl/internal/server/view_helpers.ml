open Cd_All
open Proj_common
open Strings.Latin1

(* to export it to views *)
let ( & ) = Proj_common.( ( & ) )

(* [a1] has priority, overrides attributes from [a2].
   except for classes, where class names are glued (a1's first, then a2's).
 *)
let merge_attrs a1 a2 =
  let m = StrMap.empty in
  let merge m (k, v2) =
    let add_opt =
      match strmap_find_opt k m with
      | None -> Some v2
      | Some v1 ->
          if k = "class"
          then
            Some (v1 ^ " " ^ v2)
          else
            None
    in
    match add_opt with
    | None -> m
    | Some v -> StrMap.add k v m
  in
  let m = List.fold_left merge m a1 in
  let m = List.fold_left merge m a2 in
  StrMap.fold (fun k v a -> (k, v) :: a) m []


let add_tag tag attrs add_body buf =
  Buffer.add_string buf "<";
  Buffer.add_string buf tag;
  List.iter
    (fun (an, av) ->
       Buffer.add_char buf ' ';
       Buffer.add_string buf an;
       Buffer.add_string buf "=\"";
       buffer_add_html buf av;
       Buffer.add_char buf '"'
    )
    attrs;
  Buffer.add_char buf '>';
  add_body buf;
  Buffer.add_string buf "</";
  Buffer.add_string buf tag;
  Buffer.add_char buf '>'

let link_to txt url =
  let buf = Buffer.create 100 in
  add_tag "a" [("href", url)] (fun buf -> buffer_add_html buf txt) buf;
  Buffer.contents buf

let image_tag ?(attrs = []) ?(classes = []) ?alt url =
  let buf = Buffer.create 100 in
  let gen_attrs =
    [("src", url)]
    @ begin match alt with
      | None -> []
      | Some txt -> [("alt", txt)]
      end
    @ begin
        if classes = []
        then []
        else [("class", String.concat " " classes)]
      end
  in
  add_tag "img" (merge_attrs attrs gen_attrs) (fun _ -> ()) buf;
  Buffer.contents buf


let name_for_class field_name = String.replace_char '.' '-' field_name

type textarea = [ `Rows of int ]

let add_input_field_nolabel ~typ ?textarea name v buf =
  match (textarea : textarea option) with
  | None ->
      add_tag "input"
        [ ("name", name)
        ; ("type", typ)
        ; ("value", v)
        ; ("class", "form-input form-input-" ^ name_for_class name)
        ; ("id", name)
        ]
        (fun _buf -> ()  (* no contents *) )
        buf
  | Some (`Rows rows) ->
      add_tag "textarea"
        [ ("name", name)
        ; ("rows", string_of_int rows)
        ; ("id", name)
        ]
        (fun buf -> buffer_add_html buf v)
        buf

let add_input_field ?(outer_attrs=[]) ~typ ?label ?textarea name v buf =
  let raw =
    add_tag "div"
      [("class", "form-control form-control-" ^ name_for_class name)]
      (add_input_field_nolabel ~typ ?textarea name v)
  in
  if typ = "hidden"
  then
    add_tag "div"
      [("style", "margin:0;padding:0;display:inline")]
      raw buf
  else if typ = "submit"
  then
    raw buf
  else
    add_tag "div"
      (merge_attrs outer_attrs
        [("class", "form-group form-group-" ^ name_for_class name)]
      )
      (fun buf ->
         let label_text =
           match label with
           | None ->
               String.concat " " &
               String.split ( ( = ) '_' ) &
               String.capitalize
                 (let (_prefix, dot, rest) =
                    String.split_by_first ((=) '.') name in
                    if dot = "" then name else rest
                 )
           | Some l -> l
         in
         add_tag "label"
           [ ("class", "form-label form-label-" ^ name_for_class name)
           ; ("for", name)
           ]
           (fun buf -> buffer_add_html buf label_text)
           buf;
         raw buf
      )
      buf

let add_submit_button label = add_input_field ~typ:"submit" "submit" label


(* todo: добавить js-вопрос "точно хотите нажать кнопку?".
   посмотреть на хтмл-атрибуты data-*.
   вероятно, добавить в layout какой-то жс, который на их основании
   будет добавлять onclick или подобное.
 *)
let button_to label path =
  let buf = Buffer.create 100 in
  begin
    add_tag "form"
      [ ("accept-charset", "UTF-8")
      ; ("method", "post")
      ; ("action", path)
      ]
      (add_tag "div" []
         (add_tag "input"
            [ ("value", label)
            ; ("type", "submit")
            ]
            (fun _ -> ())
         )
      )
    buf
  end;
  Buffer.contents buf

class type ['a] field_desc
 =
  object
    method v : 'a form_field
    method to_form : 'a -> string
  end

let format_field
 : 'a . ?format:('a -> string) -> 'a #field_desc -> string
 = fun ?format field_desc ->
  match field_desc#v with
  | Form_field_typed x ->
      (match format with
       | None -> field_desc#to_form
       | Some f -> f
      )
      x
  | Form_field_string s -> s


let form_for ~create ~edit strobj buf build : unit =
  let id_str = format_field ~format:Int64.to_string strobj#id in
  add_tag "form"
    [ ("accept-charset", "UTF-8")
    ; ("method", "post")
    ; ( "action"
      , let is_new = (id_str = "" || id_str = "0") in
        if is_new then create else Lazy.force edit
      )
    ]
    (fun buf ->
       let builder =
         object
           method input : 'a . ?typ:string -> ?label:string ->
               ?textarea:textarea -> ?format:('a -> string) ->
               'a #field_desc -> _
             = fun ?(typ="text") ?label ?textarea ?format field_desc ->
             let str = format_field ?format field_desc in
             add_input_field
               ~outer_attrs:
                 (if field_desc#errors = ""
                  then []
                  else [("class", "form-field-error")]
                 )
               ~typ ?label ?textarea field_desc#form_name str buf
           method submit label =
             add_submit_button label buf
           method error field_desc =
             let err = field_desc#errors in
             if err = ""
             then ()
             else add_tag "p" [] (fun buf -> buffer_add_html buf err) buf
         end
       in
       builder#input strobj#id ~typ:"hidden";
       build builder strobj
    )
    buf


let truncate len txt =
  let open Strings.Utf8 in
  if String.length txt > len
  then String.sub txt 0 len ^ "…"
  else txt
