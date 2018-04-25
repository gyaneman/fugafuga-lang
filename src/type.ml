exception Unknown_type_descriptor
(*exception Unknown_type_id*)

type type_desc = { sym : string ; id : int }

let extend_type_env (type_id:string) (type_env:type_desc list) =
  match type_env with
  | [] -> { sym=type_id ; id=0 } :: type_env
  | { sym=s ; id=i } :: type_env_ -> { sym=type_id ; id=i+1 } :: type_env

let rec apply_type_env type_env type_sym =
  match type_env with
  | [] -> raise Unknown_type_descriptor 
  | x :: type_env_ when x.sym = type_sym -> x
  | x :: type_env_ -> apply_type_env type_env_ type_sym

let extend_type_env_if_no_exists (type_sym:string) (type_env:type_desc list) =
  let rec f ts te =
    match te with
    | [] -> extend_type_env type_sym type_env
    | x :: te_ when x.sym = type_sym -> type_env
    | x :: te_ -> f ts te_
  in
  f type_sym type_env


(*
type typedesc = { typeid: int }
type type_table_cell = { name: string; typeid: int }

let type_table = ref
  [
    { name="string"; typeid=2 };
    { name="bool"; typeid=1 };
    { name="int"; typeid=0 }
  ]

let extend_type_table newtype =
  match !type_table with
  | [] ->
      let typeid = 0 in
      type_table := { name=newtype; typeid=0 } :: !type_table;
      { typeid=typeid }
  | cell :: type_table_ ->
      let lastid = cell.typeid in
      type_table := { name=newtype; typeid=(lastid+1) } :: !type_table;
      { typeid=(lastid+1) }
;;

let rec search_from_type_table typestr type_table_ =
  match type_table_ with
  | [] -> raise Unknown_type_descriptor
  | cell :: type_table_ ->
      if cell.name = typestr then
        { typeid=cell.typeid }
      else
        search_from_type_table typestr type_table_
;;

let get_typedesc typestr =
  search_from_type_table typestr !type_table
;;

let rec get_type_from_typedesc_ id = function
  | [] -> raise Unknown_type_id
  | cell :: type_table_ ->
      if cell.typeid = id then
        cell.name
      else
        get_type_from_typedesc_ id type_table_
;;

let get_type_from_typedesc (td:typedesc) =
  get_type_from_typedesc_ td.typeid !type_table
;;
*)
