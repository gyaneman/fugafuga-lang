(* interp.ml *)

open Ast
open Value
open Environment

exception Type_error
exception Not_implemented_yet
exception Call_stack_error

type cont =
  | NEXT (* execute next *)
  | RET (* when `ret` statement is executed *)
  | CONTINUE (* when `continue` statement is executed *)
  | BREAK (* when `break` statement is executed*)


(* for the return value of functions *)
let retval = ref NullVal;;


let rec interp prog env =
  retval := NullVal;
  interp_stmts (prog.program) env;
  !retval
and interp_stmts stmtlist env =
  match stmtlist with
  | [] -> NEXT
  | stmt :: stmtlist_ ->
      let new_env = ref env in
      let continuation =
      match stmt with
      | Block (stmtlist__) ->
          interp_stmts stmtlist__ env
      | For (init, cond, update, body) ->
          eval init env;
          forloop cond update body env
      | If (cond, conseq, alter) ->
          match (eval cond env) with
          | BoolVal (b) ->
              if b then
                interp_stmts conseq env
              else
                interp_stmts alter env
          | _ -> raise Type_error;
          ;
      | Expression (exp) ->
          eval exp env;
          NEXT
      | VarDecl (id, exp) ->
          let exp_val = eval exp env in
          new_env := extend_env id exp_val env;
          NEXT
      | Func (id, params, body) ->
          new_env := extend_env id (FuncVal (params, body)) env;
          NEXT
      | Ret (exp) ->
          retval := eval exp env;
          RET
      in
      match continuation with
      | NEXT -> interp_stmts stmtlist_ !new_env
      | _ -> continuation
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
          let left_int = intval_to_int left_val in
          let right_int = intval_to_int right_val in
          BoolVal (left_int = right_int)
      | NotEqual ->
          let left_int = intval_to_int left_val in
          let right_int = intval_to_int right_val in
          BoolVal (not (left_int = right_int))
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
  | Call (f, args) ->
      let f_val = eval f env in
      match f_val with
      | FuncVal (params, body) ->
          let arg_vals = eval_list args env in
          let new_env = extend_env_params params arg_vals env in
          retval := NullVal;
          interp_stmts body new_env;
          !retval
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
      eval exp env :: eval_list explist_ env;
  ;
and forloop cond update body env =
  if boolval_to_bool (eval cond env) then
    match interp_stmts body env with
    | BREAK -> NEXT
    | RET -> RET
    | _ ->
        eval update env;
        forloop cond update body env
  else
    NEXT
;;


