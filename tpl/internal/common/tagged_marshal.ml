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

let to_file ?(flags = []) ~tag fname v =
  if String.contains tag '\n'
  then invalid_arg "Tagged_marshal.to_file: tag must not contain newlines"
  else
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
