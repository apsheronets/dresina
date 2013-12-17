(** Application's routes *)

open Route_lib

(* resources :users, :as => users *) (* рельсовый аналог: *)
(* :as => users, resources :users *) (* так понятнее *)
let users = resources "users"
let say_hello = path "say/hello"
