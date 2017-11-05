(* environment.ml *)
open Value

exception Not_found_the_identifier_in_the_env

type env_elem = { sym: string; valp: Value.value ref }

let extend_env id v env =
  { sym=id; valp=ref v } :: env
;;

let rec apply_env env id =
  match env with
  | [] -> raise Not_found_the_identifier_in_the_env
  | el :: env_ -> if el.sym = id then !(el.valp) else apply_env env_ id
;;

let print_env_elem el =
  print_string ("id: " ^ el.sym ^ ", val: " ^ string_of_value (!(el.valp)) ^ "\n")
;;

let rec print_env_ i = function
  | [] -> print_string "==== end_of_env ====\n";
  | el :: env_ ->
      print_string (string_of_int i ^ ": ");
      print_env_elem el;
      print_env_ (i+1) env_
;;

let rec print_env env = print_env_ 0 env;;
