open Identifier


exception Unknown_type_descriptor
exception Type_environment_internal_error
(*exception Unknown_type_id*)

type type_desc = { sym : string ; id : int }

type type_info =
  | TypeInfoVar of type_desc (* type of variable *)
  | TypeInfoFunc of type_desc * type_desc list (* type of return value * types of params *)

type type_environment_cell = {
  id: identifier;
  type_info: type_info;
}

type type_environment = type_environment_cell list

let init_type_environment:type_environment = []

let prim_type_string = { sym="string"; id=2 }
let prim_type_bool   = { sym="bool"; id=1 }
let prim_type_int    = { sym="int"; id=0 }

let type_desc_set = ref (
  prim_type_string ::
  prim_type_bool ::
  prim_type_int :: [])

let untyped = { sym="untyped"; id=(-1) }

let get_type_desc (type_sym:string) =
  let rec f tds =
    match tds with
    | [] ->
        begin
          let new_type_desc =
            let id = match !type_desc_set with
            | x :: xs -> x.id + 1
            | _ -> raise Type_environment_internal_error
            in { sym=type_sym; id=id }
          in
          let new_type_desc_set = new_type_desc :: !type_desc_set in
          type_desc_set := new_type_desc_set;
          new_type_desc
        end
    | x :: tds_ when x.sym = type_sym -> x
    | x :: tds_ -> f tds_
  in
  f !type_desc_set
;;

let extend_type_env (id:identifier) (t:type_info) (type_env:type_environment) =
  { id=id; type_info=t } :: type_env;;

let rec apply_type_env (type_env:type_environment) (id:identifier) =
  match type_env with
  | [] -> raise Unknown_type_descriptor
  | cell :: type_env_ when cell.id = id -> cell.type_info
  | cell :: type_env_ -> apply_type_env type_env_ id

