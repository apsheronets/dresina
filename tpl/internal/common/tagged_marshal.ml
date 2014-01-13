let invalid_arg fmt = Codegen.invalid_arg fmt

let with_file_gen fopen fclose fn func =
  let ch = fopen fn in
  let finally () = fclose ch in
  try
    let r = func ch in
    let () = finally () in
    r
  with
    e -> (finally (); raise e)

let with_file_in_bin fn func = with_file_gen open_in_bin close_in fn func
let with_file_out_bin fn func = with_file_gen open_out_bin close_out fn func

let check_tag ~place ~tag =
  if String.contains tag '\n'
  then invalid_arg "Tagged_marshal.%s: tag must not contain newlines" place
  else ()

let to_file ?(flags = []) ~tag fname v =
  check_tag ~place:"to_file" ~tag;
  with_file_out_bin fname begin fun ch ->
    output_string ch (tag ^ "\n");
    Marshal.to_channel ch v flags
  end

let from_file ~tag fname =
  with_file_in_bin fname begin fun ch ->
    let tag_read = input_line ch in
    if tag <> tag_read
    then Pervasives.failwith (Printf.sprintf
      "Tagged_marshal.from_file: expected tag %S, read tag %S"
      tag tag_read)
    else
      Marshal.from_channel ch
  end

let to_string ?(flags = []) ~tag v =
  check_tag ~place:"to_string" ~tag;
  tag ^ "\n" ^
  Marshal.to_string v flags

module Make (T : sig type t val tag : string end)
 =
  struct
    open T
    let to_file ?(flags = []) fn (v : t) = to_file ~flags ~tag fn v
    let from_file fn = ((from_file ~tag fn) : t)
    let to_string ?(flags = []) (v : t) = to_string ~flags ~tag v
  end
