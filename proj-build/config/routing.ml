# 3 "proj/config/routes.mlt"

# 0 "_routing_generated_"
let routes path =
  begin match path with
  | "myint" :: path ->
      begin match path with
      | [] ->
          raise Not_found
      | __uri_patt_1 :: path ->
          begin match path with
          | "mystring" :: path ->
              begin match path with
              | [] ->
                  raise Not_found
              | __uri_patt_3 :: path ->
                  begin match path with
                  | [] | "" :: [] ->
                      Test_controller.test_action ~int_arg:((try int_of_string __uri_patt_1 with Failure _ -> raise Not_found)) ~string_arg:(__uri_patt_3)
                  | _ ->
                      raise Not_found
                  end
              end
          | _ ->
              raise Not_found
          end
      end
  | "say" :: path ->
      begin match path with
      | "goodbye" :: path ->
          begin match path with
          | [] | "" :: [] ->
              Say_controller.goodbye
          | _ ->
              raise Not_found
          end
      | "hello" :: path ->
          begin match path with
          | [] | "" :: [] ->
              Say_controller.hello
          | _ ->
              raise Not_found
          end
      | _ ->
          raise Not_found
      end
  | _ ->
      raise Not_found
  end
;;
