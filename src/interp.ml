(* interp.ml *)

open Ast
open Value
open Environment

exception Type_error
exception Not_implemented_yet


let rec interp prog env =
  interp_stmts (prog.program) env
and interp_stmts stmtlist env =
  match stmtlist with
  | [] -> ()
  | stmt :: stmtlist_ ->
      let new_env = match stmt with
      | Block (stmtlist__) -> interp_stmts stmtlist__ env; env
      | For (init, cond, update, body) ->
          raise Not_implemented_yet
      | If (cond, conseq, alter) ->
          match (eval cond env) with
          | BoolVal (b) ->
              if b then begin
                interp_stmts conseq env; env;
              end else begin
                interp_stmts alter env; env;
              end
          | _ -> raise Type_error;
          ;
      | Expression (exp) ->
          print_value (eval exp env); env;
      in
      interp_stmts stmtlist_ new_env
  ;
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
      match lit with
      | Null -> NullVal
      | Int (num) -> IntVal (num)
      | Bool (b) -> BoolVal (b)
      | String (str) -> StringVal (str)
  | _ -> raise Not_implemented_yet
;;


