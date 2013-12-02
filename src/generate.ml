(* generate files, compile them, etc *)

module type GEN_ENV
 =
  sig
  end


open Make


module Generate (Env : GEN_ENV)
 =
  struct

    open Env;;

    make ....  тут как-то модно указывать, где tpl, где proj, где proj_out(?)

  end
