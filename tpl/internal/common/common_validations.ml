open Cd_All
open Strings.Latin1

let float_greater_than n x =
  let () = Printf.eprintf "val: %f must be greater than %f\n%!" x n in
  if x > n
  then ()
  else failwith (Printf.sprintf "must be greater than %f" n)

let is_whitespace = function
| '\x20' | '\x09' | '\n' | '\r' -> true
| _ -> false

let not_blank s =
  if String.for_all is_whitespace s
  then failwith "is blank"
  else ()
