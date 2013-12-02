type field =
  { f_name : string
  ; f_type : string
  }

type sum_constr =
  { c_name : string
  ; c_arg_types : string list
  }

type ty =
| Record_type of string * field list
| Sum_type of string * sum_constr list

type context =
  { mutable cur_type : ty option
  ; mutable finished_type : ty option
  }

let finish ctx =
  match ctx.cur_type with
  | None -> ()
  | Some ty ->
      ctx.cur_type <- None;
      match ctx.finished_type with
      | None ->
          ctx.finished_type <- Some ty
      | Some _ -> failwith "internal error in finish()"

let flush0 = finish

let record_type1 rname ctx =
  finish ctx;
  ctx.cur_type <- Some (Record_type (rname, []))

let field2 fname ftype ctx =
  match ctx.cur_type with
  | None | Some (Sum_type _) ->
      failwith "field definition must be in record definition only"
  | Some (Record_type (rname, fields)) ->
      let fields = fields @ [ { f_name = fname ; f_type = ftype } ] in
      ctx.cur_type <- Some (Record_type (rname, fields))

let sum_type1 sname ctx =
  finish ctx;
  ctx.cur_type <- Some (Sum_type (sname, []))

let cur_sum_type ctx =
  match ctx.cur_type with
  | None
  | Some (Record_type _) ->
      failwith "currently-constructed type is not a sum-type"
  | Some (Sum_type (sname, st)) -> (sname, st)

let sum_add_constr cname st =
  st @ [ { c_name = cname ; c_arg_types = [] } ]

let sum_constr1 cname ctx =
  let (sname, st) = cur_sum_type ctx in
  let st = sum_add_constr cname st in
  ctx.cur_type <- Some (Sum_type (sname, st))

let sum_add_arg atype st =
  match st with
  | [] -> failwith "sum type arguments are added inside sum type constructors"
  | h :: t ->
      let rec loop h t =
        match t with
        | [] -> [ { (h) with c_arg_types = h.c_arg_types @ [atype] } ]
        | h' :: t' -> h' :: loop h' t'
      in
        loop h t

let sum_arg1 atype ctx =
  let (sname, st) = cur_sum_type ctx in
  let st = sum_add_arg atype st in
  ctx.cur_type <- Some (Sum_type (sname, st))
