
(** Useful combinators *)

let (>>) f g = g f
let (^/) = Filename.concat
let (!!) = Lazy.force

(** применить значение к функции:
    print_string & string_of_int & 123

    NB: оператор "&" является ключевым словом в jocaml

    Если попробовать объявить "let ( $ ) f x = f x",
    то полученный оператор будет левоассоциативным,
    что нежелательно в данном случае.
*)
let ( & ) f x = f x

