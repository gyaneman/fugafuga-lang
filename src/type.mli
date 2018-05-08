open Identifier


exception Unknown_type_descriptor

type type_desc
type type_environment
type type_info =
  | TypeInfoVar of type_desc (* type of variable *)
  | TypeInfoFunc of type_desc * type_desc list (* type of return value * types of params *)

val init_type_environment: type_environment

(* `untyped' type_desc describes that an expr is untyped *)
val untyped: type_desc

val prim_type_string: type_desc
val prim_type_bool:   type_desc
val prim_type_int:    type_desc

val get_type_desc: string -> type_desc

val extend_type_env: identifier -> type_info -> type_environment -> type_environment
val apply_type_env: type_environment -> identifier -> type_info
(*val extend_type_env_if_no_exists: identifier -> type_environment -> type_environment*)

