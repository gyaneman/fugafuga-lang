exception Unknown_type_descriptor
exception Unknown_type_id

type typedesc = { typeid: int }
val extend_type_table: string -> typedesc
val get_typedesc: string -> typedesc
val get_type_from_typedesc: typedesc -> string
