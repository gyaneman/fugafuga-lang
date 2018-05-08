open Ast
open Type
open Identifier

exception Type_error


let get_type_of_expr expr =
  match expr with
  | ASTBinary (t, _, _, _) -> t
  | ASTUnary (t, _, _) -> t
  | ASTAssign (t, _, _) -> t
  | ASTCall (t, _, _) -> t
  | ASTIdentifier (t, _) -> t
  | ASTLiteral (t, _) -> t

let check_args_types args params_types =
  let rec f args params_types =
    begin 
      match args with
      | [] ->
          begin
            match params_types with
            | [] -> ()
            | _ -> raise Type_error
          end
      | arg :: args_ ->
          begin
            match params_types with
            | [] -> raise Type_error
            | pt :: params_types_ ->
                begin
                  if get_type_of_expr arg = pt then
                    f args_ params_types_
                  else raise Type_error
                end
          end
    end
  in f args params_types

let checks_func_type callee args env =
  match callee with
  | ASTIdentifier (_, id) ->
      begin
        match apply_type_env env id with
        | TypeInfoFunc (ret_type, params_types) ->
            check_args_types args params_types;
            ret_type
        | _ -> raise Type_error
      end
  | _ -> raise Type_error

let rec type_expr_list expr_list env =
  let rec f expr_list = match expr_list with
  | [] -> []
  | expr :: expr_list ->
      begin
        let (expr, _) = type_expr expr env in
        expr :: f expr_list
      end
  in f expr_list
and type_expr expr env = match expr with
  | ASTBinary (_, op, lhs, rhs) ->
      begin
        let (lhs, lhs_type) = type_expr lhs env in
        let (rhs, rhs_type) = type_expr rhs env in
        let ret_type =
          match (lhs_type, rhs_type) with
          | (a, b) when a = prim_type_int && b = prim_type_int ->
              prim_type_int
          | _ -> raise Type_error
        in
          (ASTBinary (ret_type, op, lhs, rhs), ret_type)
      end
  | ASTUnary (_, op, expr) ->
      let (expr, expr_type) = type_expr expr env in
      (ASTUnary (prim_type_bool, op, expr), prim_type_bool)
  | ASTAssign (_, id, expr) ->
      let id_type_info = apply_type_env env id in
      let (expr, expr_type) = type_expr expr env in
      begin
        match id_type_info with
        | TypeInfoVar (id_type) ->
            if id_type = expr_type then
              (ASTAssign (expr_type, id, expr), expr_type)
            else
              raise Type_error
        | _ -> raise Type_error
      end
  | ASTCall (_, callee, args) ->
      (*let (callee, _) = type_expr callee env in*)
      let args = type_expr_list args env in
      let ret_type = checks_func_type callee args env in
      (ASTCall (ret_type , callee, args), ret_type)
  | ASTIdentifier (_, id) ->
      begin
        match apply_type_env env id with
        | TypeInfoVar (id_type) -> (ASTIdentifier (id_type, id), id_type)
        | _ -> raise Type_error
      end
  | ASTLiteral (_, lit) ->
      begin
        match lit with
        | ASTLitNull -> raise Type_error
        | ASTLitInt (n) -> (ASTLiteral (prim_type_int, lit), prim_type_int)
        | ASTLitBool (b) -> (ASTLiteral (prim_type_bool, lit), prim_type_bool)
        | ASTLitString (s) ->
            (ASTLiteral (prim_type_string, lit), prim_type_string)
      end
and type_stmt stmt env ret_type_stack =
  match stmt with
  | ASTBlock (stmts) -> (ASTBlock (type_statements stmts env ret_type_stack), env)
  | ASTFor (init, test, update, stmt) ->
      begin
        let (init, _) = type_expr init env in
        let (test, _) = type_expr test env in
        let (update, _) = type_expr update env in
        let (stmt, _) = type_stmt stmt env ret_type_stack in
        (ASTFor (init, test, update, stmt), env)
      end
  | ASTBreak -> (ASTBreak, env)
  | ASTContinue -> (ASTContinue, env)
  | ASTIf (cond, conseq, alter) ->
      begin
        let (cond, _) = type_expr cond env in
        let conseq = type_statements conseq env ret_type_stack in
        let alter = type_statements alter env ret_type_stack in
        (ASTIf (cond, conseq, alter), env)
      end
  | ASTExpression (expr) ->
      begin
        let (expr, _) = type_expr expr env in
        (ASTExpression (expr), env)
      end
  | ASTVarDecl (var, initexpr) ->
      begin
        let (initexpr, _) = type_expr initexpr env in
        let env = extend_type_env var (TypeInfoVar (get_type_of_expr initexpr)) env in
        (ASTVarDecl (var, initexpr), env)
      end
  | ASTRet (expr) ->
      let (expr, expr_type) = type_expr expr env in
      match ret_type_stack with
        | [] -> raise Type_error
        | td :: ret_type_stack_ when td = expr_type ->
            (ASTRet (expr), env)
        | _ -> raise Type_error
and type_statements stmts env ret_type_stack=
  let rec f stmts env =
    match stmts with
    | [] -> []
    | x :: xs ->
        begin
          let (stmt, env) = type_stmt x env ret_type_stack in stmt :: f xs env
        end
  in f stmts env
and type_func func env ret_type_stack =
  let rec f li env =
    match li with
    | [] -> env
    | (var, td) :: li_ ->
        f li_ (extend_type_env var (TypeInfoVar (td)) env)
  in
  let body = type_statements func.stmts (f func.params env) (func.ret_type :: ret_type_stack) in
  {
    id=func.id;
    ret_type=func.ret_type;
    params=func.params;
    stmts=body
  }

  
let rec type_funcs funcs env ret_type_stack =
  match funcs with
  | [] -> []
  | func :: funcs_ -> type_func func env ret_type_stack :: type_funcs funcs_ env ret_type_stack

let extends_type_env_funcs funcs env =
  let rec f funcs env =
    match funcs with
    | [] -> env
    | func :: funcs_ ->
        begin
          let params_types =
            let rec extract_type_desc_list li = match li with
            | [] -> []
            | (id, td) :: li_ -> td :: extract_type_desc_list li_
            in extract_type_desc_list func.params
          in
          f funcs_ (extend_type_env func.id (TypeInfoFunc (func.ret_type, params_types)) env)
        end
  in f funcs env


let type_program program type_env =
  let type_env = extends_type_env_funcs program.functions type_env in
  let typed_stmts = type_statements program.statements type_env [] in
  let typed_funcs = type_funcs program.functions type_env [] in
  { functions=typed_funcs; statements=typed_stmts }
  
