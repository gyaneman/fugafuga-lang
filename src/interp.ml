(* interp.ml *)

open Ast
open Value
open Environment

exception Type_error
exception Not_implemented_yet
exception Call_stack_error


(* for the return value of functions *)
let retval = ref NullVal;;


let rec interp prog env =
  interp_stmts (prog.program) env
and interp_stmts stmtlist env =
  match stmtlist with
  | [] -> true
  | stmt :: stmtlist_ ->
      let (is_continue, new_env) =
      match stmt with
      | Block (stmtlist__) ->
          (interp_stmts stmtlist__ env, env);
      | For (init, cond, update, body) ->
          raise Not_implemented_yet
      | If (cond, conseq, alter) ->
          match (eval cond env) with
          | BoolVal (b) ->
              if b then
                (interp_stmts conseq env, env)
              else
                (interp_stmts alter env, env)
          | _ -> raise Type_error;
          ;
      | Expression (exp) ->
          (true, env)
      | VarDecl (id, exp) ->
          let exp_val = eval exp env in
          (true, extend_env id exp_val env)
      | Ret (exp) ->
          retval := eval exp env;
          (false, env);
      in
      if is_continue then
        interp_stmts stmtlist_ new_env
      else
        false
and eval exp env =
  match exp with
  | Binary (op, left, right) ->
      begin
      let left_val = eval left env in
      let right_val = eval right env in
      match op with
      | Add ->
          let left_int = intval_to_int left_val in
          let right_int = intval_to_int right_val in
          IntVal (left_int + right_int)
      | Sub ->
          let left_int = intval_to_int left_val in
          let right_int = intval_to_int right_val in
          IntVal (left_int - right_int)
      | Mul ->
          let left_int = intval_to_int left_val in
          let right_int = intval_to_int right_val in
          IntVal (left_int * right_int)
      | Div ->
          let left_int = intval_to_int left_val in
          let right_int = intval_to_int right_val in
          IntVal (left_int / right_int)
      | Mod ->
          let left_int = intval_to_int left_val in
          let right_int = intval_to_int right_val in
          IntVal (left_int mod right_int)
      | Equal ->
          raise Not_implemented_yet
      | NotEqual ->
          raise Not_implemented_yet
      | LT ->
          let left_int = intval_to_int left_val in
          let right_int = intval_to_int right_val in
          BoolVal (left_int < right_int)
      | LTE ->
          let left_int = intval_to_int left_val in
          let right_int = intval_to_int right_val in
          BoolVal (left_int <= right_int)
      | GT ->
          let left_int = intval_to_int left_val in
          let right_int = intval_to_int right_val in
          BoolVal (left_int > right_int)
      | GTE ->
          let left_int = intval_to_int left_val in
          let right_int = intval_to_int right_val in
          BoolVal (left_int >= right_int)
      end
  | Unary (op, e) ->
      let e_val = eval e env in
      match op with
      | Not -> BoolVal (not (boolval_to_bool e_val));
      ;
  | Literal (lit) ->
      begin
        match lit with
        | Null -> NullVal
        | Int (num) -> IntVal (num)
        | Bool (b) -> BoolVal (b)
        | String (str) -> StringVal (str)
      end
  | Func (params, body) ->
      FuncVal (params, body)
  | Call (f, args) ->
      let f_val = eval f env in
      match f_val with
      | FuncVal (params, body) ->
          let arg_vals = eval_list args env in
          let new_env = extend_env_params params arg_vals env in
          retval := NullVal;
          interp_stmts body new_env;
          !retval;
      | _ -> raise Type_error;
      ;
  | Assign (id, exp) ->
      let exp_val = eval exp env in
      assign_env id exp_val env;
      exp_val
  | Ident (id) ->
      apply_env env id
and eval_list explist env =
  match explist with
  | [] -> []
  | exp :: explist_ ->
      eval exp env :: eval_list explist_ env
;;


