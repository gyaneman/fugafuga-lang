(* environment.ml *)
open Value

exception Not_found_the_identifier_in_the_env
exception Failed_to_extend_env

type env_elem = { sym: string; valp: Value.value ref }

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


let extend_env id v env =
  { sym=id; valp=ref v } :: env
;;

let rec extend_env_params params args env =
  match params with
  | [] -> []
  | p :: params_ ->
      match args with
      | [] -> raise Failed_to_extend_env
      | a :: args_ ->
          extend_env p a (extend_env_params params_ args_ env)
;;

let rec apply_env env id =
  match env with
  | [] -> print_string id; print_newline(); raise Not_found_the_identifier_in_the_env
  | el :: env_ -> if el.sym = id then !(el.valp) else apply_env env_ id
;;

let rec assign_env id v env =
  match env with
  | [] -> raise Not_found_the_identifier_in_the_env
  | el :: env_ -> if el.sym = id then el.valp := v else assign_env id v env_
;;


