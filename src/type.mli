exception Unknown_type_descriptor

type type_desc = { sym : string ; id : int }

val extend_type_env: string -> type_desc list -> type_desc list
val apply_type_env: type_desc list -> string -> type_desc
val extend_type_env_if_no_exists: string -> type_desc list -> type_desc list

(*
type typedesc = { typeid: int }
val extend_type_table: string -> typedesc
val get_typedesc: string -> typedesc
val get_type_from_typedesc: typedesc -> string
*)
