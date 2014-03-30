open Cd_All
open Strings.Latin1
open Migrate_types

let int64__default _ctm = Int64.of_string

let string__default _ctm =
  (* until strings don't contain length db constraints, it's *)
  identity


let is_space = function ' ' | '\t' -> true | _ -> false

let is_dot c = (c = '.')

let float_gen rounding ctm =
  match ctm with
  | Ctm_none -> assert false  (* scale/precision must be specified *)
  | Ctm_decimal (precision, scale) -> fun str ->
      if scale >= 0
      then
        Input_float.of_string ~precision ~scale ~is_space ~is_dot ~rounding str
      else
        assert false (* temporary *)

let float__round = float_gen Input_float.Round_half_up
let float__truncate = float_gen Input_float.Round_to_zero
let float__exact = float_gen Input_float.Round_fail

let float__default = float__round
