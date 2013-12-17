
open Printf

let render ?(b = 9999) f =
  let b = Buffer.create b in
  let () = f b in
  Buffer.contents b

let urldecode = Cd_Strings.Strings.Onebyte.urldecode
let urlencode = Cd_Strings.Strings.Onebyte.urlencode
