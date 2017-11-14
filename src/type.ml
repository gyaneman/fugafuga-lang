type typedesc = { typeid: int }
type type_table_cell = { name: string; typeid: int }

let type_table = ref
  { name="string", typeid=2 } ::
    { name="bool", typeid=1 } ::
      { name="int", typeid=0 } :: []
;;

let extend_type_table newtype =
  match !type_table with
  | [] -> type_table := { name=newtype, typeid=0 }; 0
  | cell :: type_table_ ->
      let lastid = cell.typeid in
      type_table := { name=newtype, typeid=(lastid+1) };
      lastid + 1
;;

let rec search_from_type_table typestr type_table_ =
  match type_table_ with
  | [] ->
      extend_type_table typestr
  | cell :: type_table_ ->
      if cell.name = typestr then
        cell.typeid
      else
        search_from_type_table typestr type_table_
;;

let get_typedesc typestr =
  { typeid=search_from_type_table typestr !type_table }
;;

(* not implemented
let rec get_type_from_typedesc_ td;;
*)

let get_type_from_typedesc td =
  get_type_from_typedesc_ td !type_table
;;

