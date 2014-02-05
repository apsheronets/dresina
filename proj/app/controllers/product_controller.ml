(* todo: move to proj_common *)
let save_form f x ~on_error ~on_ok =
  match f x with
  | `Instance p ->
      p#save () >>= begin function
      | `Ok () -> on_ok ()
      | `Error exn -> on_error ((p#to_form : ?exn:exn -> _) ~exn ())
      end
  | `Form m ->
      on_error m


let index () =
  Product.All.load () >>= fun env ->
  Product.Index.render env


let new_with formdata =
  Product.New.render & object method product = formdata end

let new_ () =
  new_with & Product.Single.to_form `New

let redirect_to_index () =
  redirect_to Route.Product_controller.Index.path


let create () =
  save_form
    Product.Single.from_form params
    ~on_ok:redirect_to_index
    ~on_error:new_with


(* left here to show what [save_form] is. *)
let _create () =
  match Product.Single.from_form params with
  | `Instance p ->
      p#save () >>= begin function
      | `Ok () ->
          redirect_to_index ()
      | `Error exn ->
          new_with &
          p#to_form ~exn ()
      end
  | `Form m ->
      new_with m


let delete ~id () =
  Product.Single.load ~id () >>= fun p ->
  p#delete () >>= begin function
  | `Ok () ->
      redirect_to_index ()
  | `Error exn ->
      respond & sprintf "Error deleting product: %s" & Printexc.to_string exn
  end


let edit_with formdata =
  Product.Edit.render & object method product = formdata end

let edit ~id () =
  Product.Single.load ~id () >>= fun p ->
  edit_with & p#to_form ()

let update ~id () =
  Product.Single.load ~id () >>= fun p ->
  save_form
    p#update_from_form params
    ~on_ok:redirect_to_index
    ~on_error:edit_with


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
            d#map_to_list ((fun id title description image_url price ->
              let price = float_of_string (Decimal.to_string price) in
              object
                method id = id
                method title = title
                method description = description
                method image_url = image_url
                method price = price
              end)
              <$> nint64 "id" <*> nstring "title" <*> nstring "description"
              <*> nstring "image_url" <*> nnumber "price")
          end
          method iter f = List.iter f data
        end
      method products = products
    end
  end >>= fun env ->
  Product.Index.render env
