let index () =
  Product.all () >>= fun env ->
  Product.Index.render env



(* left here to show a way to manually fetch data into objects *)
let _old_index () =
  begin Database.with_connection & fun conn ->
    object
      val products =
        object
          val data = begin
            let open Dbi_pg in
            let d = expect_result_data & conn#execute
              "select * from products order by id" in
            d#map_to_list ((fun title description image_url price ->
              let price = float_of_string (Decimal.to_string price) in
              object
                method title = title
                method description = description
                method image_url = image_url
                method price = price
              end)
              <$> nstring "title" <*> nstring "description"
              <*> nstring "image_url" <*> nnumber "price")
          end
          method iter f = List.iter f data
        end
      method products = products
    end
  end >>= fun env ->
  Product.Index.render env
