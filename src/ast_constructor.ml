open Parse_tree
open Ast
open Type
open Identifier

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


let rec collect_funcs prog =
  let rec collect_funcs_ prog =
    match prog with
    | [] -> []
    | Func (id, ret_type, params, body) :: prog_ ->
        begin
          let rec f li =
            match li with
            | [] -> []
            | (id, t) :: li_ ->
                ({ sym=id }, get_type_desc t) :: f li_
                (*ASTTypedVariable (id, get_type_desc t) ::
                  str_str_list_to_identifier_list li_*)
          in 
          {
            id={ sym=id };
            ret_type=get_type_desc ret_type;
            params=f params;
            stmts=collect_stmts body;
          } :: collect_funcs_ prog_
        end
     | _ :: prog_ -> collect_funcs_ prog_
  in collect_funcs_ prog
and collect_stmts prog =
  let rec collect_stmts_ prog =
    match prog with
    | [] -> []
    | Func (_, _, _, _) :: prog_ -> collect_stmts_ prog_
    | x :: prog_ ->
        begin
          match x with
          | Block (stmts) -> ASTBlock (collect_stmts stmts)
          | For (init, test, update, stmts) ->
              ASTFor (
                make_ast_expression init,
                make_ast_expression test,
                make_ast_expression update,
                ASTBlock (collect_stmts stmts))
          | Break -> ASTBreak
          | Continue -> ASTContinue
          | If (cond, conseq, alter) ->
              ASTIf (
                make_ast_expression cond,
                collect_stmts conseq,
                collect_stmts alter 
              )
          | Expression (expr) ->
              ASTExpression (make_ast_expression expr)
          | VarDecl (var, initexpr) ->
              ASTVarDecl ({ sym=var }, make_ast_expression initexpr)
          | Ret (expr) ->
              ASTExpression (make_ast_expression expr)
          | _ -> raise Occured_error_during_converting_ast
        end :: collect_stmts_ prog_
  in collect_stmts_ prog
and make_ast_expression expr =
  match expr with
  | Binary (op, lhs, rhs) ->
      ASTBinary (untyped, cvt_binop op,
        make_ast_expression lhs,
        make_ast_expression rhs)
  | Unary (op, e) ->
      ASTUnary (untyped, cvt_unaop op, make_ast_expression e)
  | Assign (var, e) ->
      ASTAssign (untyped, { sym=var }, make_ast_expression e)
  | Call (f, args) ->
      let rec make_ast_expression_list li =
        match li with
        | [] -> []
        | e :: li_ -> make_ast_expression e :: make_ast_expression_list li_
      in 
      ASTCall (untyped, make_ast_expression f, make_ast_expression_list args)
  | Ident (id, id_kind) -> ASTIdentifier (untyped, { sym=id })
  | Literal (lit) -> ASTLiteral (untyped, make_ast_literal lit)
and make_ast_literal lit =
  match lit with
  | Null -> ASTLitNull
  | Int (n) -> ASTLitInt (n)
  | Bool (b) -> ASTLitBool(b)
  | String (s) -> ASTLitString(s)



let make_program prog =
  let funcs = collect_funcs prog.program in
  let stmts = collect_stmts prog.program in
  { functions=funcs; statements=stmts }

