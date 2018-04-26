exception Unknown_type_descriptor

type type_desc
type type_environment

val init_type_env: type_environment
val extend_type_env: string -> type_environment -> type_environment
val apply_type_env: type_environment -> string -> type_desc
val extend_type_env_if_no_exists: string -> type_environment -> type_environment 

(*
type typedesc = { typeid: int }
val extend_type_table: string -> typedesc
val get_typedesc: string -> typedesc
val get_type_from_typedesc: typedesc -> string
*)
