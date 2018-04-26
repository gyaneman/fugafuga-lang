open Type


(* unary operators *)
type ast_una_op =
  | ASTUnaOpNot
;;

(* binary operators *)
type ast_bin_op =
  | ASTBinOpAdd
  | ASTBinOpSub
  | ASTBinOpMul
  | ASTBinOpDiv
  | ASTBinOpMod
  | ASTBinOpEqual
  | ASTBinOpNotEqual
  | ASTBinOpLt
  | ASTBinOpLte
  | ASTBinOpGt
  | ASTBinOpGte
;;



type ast_identifier =
  | ASTSimpleIdentifier of string
  | ASTTypedVariable of string * type_desc
  | ASTConstant of string * type_desc
(* literals *)
and ast_literal =
  | ASTLitNull
  | ASTLitInt of int
  | ASTLitBool of bool
  | ASTLitString of string
(* expressions *)
and ast_expression =
  | ASTBinary of ast_bin_op * ast_expression * ast_expression
  | ASTUnary of ast_una_op * ast_expression
  | ASTAssign of ast_identifier * ast_expression (* dst, src *)
  | ASTCall of ast_expression * ast_expression list
  | ASTIdentifier of ast_identifier
  | ASTLiteral of ast_literal
(* statements *)
and ast_statement =
  | ASTBlock of ast_statement list
  | ASTFor of ast_expression * ast_expression * ast_expression * ast_statement
  | ASTBreak
  | ASTContinue
  | ASTIf of ast_expression * ast_statement list * ast_statement list
  | ASTExpression of ast_expression
  | ASTVarDecl of ast_identifier * ast_expression
  (* Function(id, ret datatype, param(id, datatype) list, statements) *)
  | ASTRet of ast_expression
(* function *)
and ast_function = {
  (*functions: func list;*)
  id: ast_identifier;
  ret_type: type_desc;
  args: ast_identifier list;
  stmts: ast_statement list;
}
(* program *)
and program = {
  functions: ast_function list;
  statements: ast_statement list;
}
