exception Unknown_type_descriptor
exception Unknown_type_id

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

