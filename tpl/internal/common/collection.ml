open Cd_All

type id = int64

module MapId = Map.Make(Int64)

module MapOrd = Map.Make(Int)

class ['a] map2_id_ord
 =
  object

    val mutable map_id : 'a MapId.t = MapId.empty

    val mutable map_ord : 'a MapOrd.t = MapOrd.empty

    val mutable ord_counter = ref 0

    method id_exists id = MapId.mem id map_id

    method add ?ord v =
      let id = v#id in
      if MapId.mem id map_id
      then invalid_arg "Collection: element with this id already exists"
      else
        let ord =
          match ord with
          | Some ord -> ord
          | None ->
              let ord = !ord_counter in
              ord_counter := ord - 1;
              ord
        in
          map_id <- MapId.add id v map_id;
          map_ord <- MapOrd.add ord v map_ord

    method iter f = MapOrd.iter (fun _ord v -> f v) map_ord

  end
