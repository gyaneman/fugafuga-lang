open Parse_tree
open Ast
open Type

exception Occured_error_during_converting_ast


let cvt_unaop op =
  match op with
  | Not -> ASTUnaOpNot
;;

let cvt_binop op =
  match op with
  | Add -> ASTBinOpAdd
  | Sub -> ASTBinOpSub
  | Mul -> ASTBinOpMul
  | Div -> ASTBinOpDiv
  | Mod -> ASTBinOpMod
  | Equal -> ASTBinOpEqual
  | NotEqual -> ASTBinOpNotEqual
  | LT -> ASTBinOpLt
  | LTE -> ASTBinOpLte
  | GT -> ASTBinOpGt
  | GTE -> ASTBinOpGte
;;


let rec collect_funcs prog type_env =
  let rec collect_funcs_ prog =
    match prog with
    | [] -> []
    | Func (id, ret_type, args, body) :: prog_ ->
        begin
          let rec str_str_list_to_identifier_list li =
            match li with
            | [] -> []
            | (id, t) :: li_ ->
                ASTTypedVariable (id, apply_type_env type_env t) ::
                  str_str_list_to_identifier_list li_
          in 
          {
            id=ASTSimpleIdentifier (id);
            ret_type=apply_type_env type_env ret_type;
            args=str_str_list_to_identifier_list args;
            stmts=collect_stmts body type_env;
          } :: collect_funcs_ prog_
        end
     | _ :: prog_ -> collect_funcs_ prog_
  in collect_funcs_ prog
and collect_stmts prog type_env =
  let rec collect_stmts_ prog =
    match prog with
    | [] -> []
    | Func (_, _, _, _) :: prog_ -> collect_stmts_ prog_
    | x :: prog_ ->
        begin
          match x with
          | Block (stmts) -> ASTBlock (collect_stmts stmts type_env)
          | For (init, test, update, stmts) ->
              ASTFor (
                make_ast_expression init,
                make_ast_expression test,
                make_ast_expression update,
                ASTBlock (collect_stmts stmts type_env))
          | Break -> ASTBreak
          | Continue -> ASTContinue
          | If (cond, conseq, alter) ->
              ASTIf (
                make_ast_expression cond,
                collect_stmts conseq type_env,
                collect_stmts alter  type_env
              )
          | Expression (expr) ->
              ASTExpression (make_ast_expression expr)
          | VarDecl (var, initexpr) ->
              ASTVarDecl (ASTSimpleIdentifier (var), make_ast_expression initexpr)
          | Ret (expr) ->
              ASTExpression (make_ast_expression expr)
          | _ -> raise Occured_error_during_converting_ast
        end :: collect_stmts_ prog_
  in collect_stmts_ prog
and make_ast_expression expr =
  match expr with
  | Binary (op, lhs, rhs) ->
      ASTBinary (cvt_binop op,
        make_ast_expression lhs,
        make_ast_expression rhs)
  | Unary (op, e) ->
      ASTUnary (cvt_unaop op, make_ast_expression e)
  | Assign (var, e) ->
      ASTAssign (ASTSimpleIdentifier (var), make_ast_expression e)
  | Call (f, args) ->
      let rec make_ast_expression_list li =
        match li with
        | [] -> []
        | e :: li_ -> make_ast_expression e :: make_ast_expression_list li_
      in 
      ASTCall (make_ast_expression f, make_ast_expression_list args)
  | Ident (id, id_kind) -> ASTIdentifier (ASTSimpleIdentifier (id))
  | Literal (lit) -> ASTLiteral (make_ast_literal lit)
and make_ast_literal lit =
  match lit with
  | Null -> ASTLitNull
  | Int (n) -> ASTLitInt (n)
  | Bool (b) -> ASTLitBool(b)
  | String (s) -> ASTLitString(s)



let make_program prog type_env =
  let funcs = collect_funcs prog.program type_env in
  let stmts = collect_stmts prog.program type_env in
  { functions=funcs; statements=stmts }

