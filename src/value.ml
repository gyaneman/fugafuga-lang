(* value.ml *)
exception Value_type_error

type value =
  | NullVal
  | IntVal of int
  | BoolVal of bool
  | StringVal of string
  | FuncVal of string list * Parse_tree.statement list
;;

let string_of_value = function
  | NullVal -> "Null"
  | IntVal (n) -> "Int(" ^ string_of_int n ^ ")"
  | BoolVal (b) -> "Bool(" ^ string_of_bool b ^ ")"
  | StringVal (s) -> "String(" ^ s ^ ")"
  | FuncVal (params, body) -> "Func(...)"
;;

let intval_to_int = function
  | IntVal (n) -> n
  | _ -> raise Value_type_error
;;

let boolval_to_bool = function
  | BoolVal (b) -> b
  | _ -> raise Value_type_error
;;

let print_value v = print_string ((string_of_value v) ^ "\n");;

