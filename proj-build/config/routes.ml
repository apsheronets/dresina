# 3 "proj/config/routes.mlt"

# 0 "_routes_generated_"
let routes path =
  begin match path with
  | "shit" :: path ->
      begin match path with
      | [] ->
          raise Not_found
      | __uri_patt_1 :: path ->
          begin match path with
          | "fuck" :: path ->
              begin match path with
              | [] ->
                  raise Not_found
              | __uri_patt_3 :: path ->
                  begin match path with
                  | [] | "" :: [] ->
                      Contr.meth ~myint:((try int_of_string __uri_patt_1 with Failure _ -> raise Not_found)) ~mystring:(__uri_patt_3)
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
              Say.goodbye
          | _ ->
              raise Not_found
          end
      | "hello" :: path ->
          begin match path with
          | [] | "" :: [] ->
              Say.hello
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
