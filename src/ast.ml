(* ast.ml *)
open Type

(* literals *)
type literal =
  | Null
  | Int of int
  | Bool of bool
  | String of string
;;

(* unary operators *)
type una_op =
  | Not
;;

(* binary operators *)
type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Equal
  | NotEqual
  | LT
  | LTE
  | GT
  | GTE
;;

type ident_kind =
  | Variable
;;

(* expressions *)
type expression =
  | Binary of bin_op * expression * expression
  | Unary of una_op * expression
  | Assign of string * expression (* dst, src *)
  | Call of expression * expression list
  | Ident of string * ident_kind
  | Literal of literal
(* statements *)
and statement =
  | Block of statement list
  | For of expression * expression * expression * statement list
  | Break
  | Continue
  | If of expression * statement list * statement list
  | Expression of expression
  | VarDecl of string * expression
  (* Function(id, ret datatype, param(id, datatype) list, statements) *)
  | Func of string * string * ((string * string) list) * statement list
  | Ret of expression
(* program *)
and program = { program: statement list }
;;


(* for metainfo *)
type pos = { fname: string; lnum: int; bol: int; cnum: int };;
type meta = { start_p: pos; end_p: pos };;
let string_of_pos { fname = fn; lnum = ln; bol = bo; cnum = cn } =
  "(fname:" ^ fn ^ ", lnum:" ^ string_of_int ln ^
  ", bol:" ^ string_of_int bo ^ ", cnum:" ^ string_of_int cn ^ ")"
;;
let string_of_meta { start_p = sp; end_p = ep } =
  "(start_p:" ^ string_of_pos sp ^ ", end_p:" ^ string_of_pos ep ^ ")"
;;



(* pretty printer for AST *)
let strlit s = "\"" ^ s ^ "\"";;
let prop pn valstr =
  strlit pn ^ ":" ^ valstr
let kind s = prop "kind" (strlit s)

let rec string_of_program pg =
  "{" ^
  kind "Program" ^ "," ^
  prop "program" ("[" ^ string_of_statement_list pg.program ^ "]")
  ^ "}"
and string_of_statement statement = 
  "{" ^
  match statement with
  | Block (stmtlist) ->
      kind "Block" ^ "," ^
      prop "statement_list" ("[" ^ string_of_statement_list stmtlist ^ "]")
  | For (init, cond, update, body) ->
      kind "For" ^ "," ^
      prop "init" (string_of_expression init) ^ "," ^
      prop "cond" (string_of_expression cond) ^ "," ^
      prop "update" (string_of_expression update) ^ "," ^
      prop "body" ("[" ^ string_of_statement_list body ^ "]")
  | Break ->
      kind "Break"
  | Continue ->
      kind "Continue"
  | If (exp, conseq, alter) ->
      kind "If" ^ "," ^
      prop "condition" (string_of_expression exp) ^ "," ^
      prop "conseq" ("[" ^ string_of_statement_list conseq ^ "]") ^ "," ^
      prop "alter" ("[" ^ string_of_statement_list alter ^ "]")
  | Expression (exp) ->
      kind "Expression" ^ "," ^
      prop "exp" (string_of_expression exp)
  | VarDecl (dst_id, src_exp) ->
      kind "VarDecl" ^ "," ^
      prop "destination" (strlit dst_id) ^ "," ^
      prop "source" (string_of_expression src_exp)
  | Func (id, t, params, stmts) ->
      kind "Func" ^ "," ^
      prop "id" (strlit id) ^ "," ^
      prop "type" (strlit t) ^ "," ^
      prop "params" ("[" ^ string_of_param_list params ^ "]") ^ "," ^
      prop "stmts" ("[" ^ string_of_statement_list stmts ^ "]")
  | Ret (exp) ->
      kind "Func" ^ "," ^
      prop "exp" (string_of_expression exp)
  ;
  ^ "}"
and string_of_statement_list stmtlist =
  match stmtlist with
  | [] -> ""
  | stmt :: stmtlist_ ->
      string_of_statement stmt ^
      string_of_statement_list_ stmtlist_
  ;
and string_of_statement_list_ = function
  | [] -> ""
  | stmtlist -> "," ^ string_of_statement_list stmtlist
  ;
and string_of_expression_list exprlist =
  match exprlist with
  | [] -> ""
  | expr :: exprlist_ ->
      string_of_expression expr ^
      string_of_expression_list_ exprlist_
  ;
and string_of_expression_list_ = function
  | [] -> ""
  | exprlist -> "," ^ string_of_expression_list exprlist
  ;
and string_of_expression exp =
  "{" ^
  match exp with
  | Binary (op, left, right) ->
      kind "BinaryExp" ^ "," ^
      prop "operator" (strlit (string_of_binop op)) ^ "," ^
      prop "left" (string_of_expression left) ^ "," ^
      prop "right" (string_of_expression right)
  | Unary (op, e) ->
      kind "UnaryExp" ^ "," ^
      prop "operator" (strlit (string_of_unaop op)) ^ "," ^
      prop "exp" (string_of_expression e)
  | Assign (dst, src) ->
      kind "AssignExp" ^ "," ^
      prop "destination" (strlit dst) ^ "," ^
      prop "source" (string_of_expression src)
  | Call (f, args) ->
      kind "CallExp" ^ "," ^
      prop "f" (string_of_expression f) ^ "," ^
      prop "args" ("[" ^ string_of_expression_list args ^ "]")
  | Ident (id, _) ->
      kind "Ident" ^ "," ^
      prop "id" (strlit id) ^ "," ^
      prop "ident_kind" (strlit "Variable")
  | Literal (lit) ->
      kind "Literal" ^ "," ^
      prop "literal" (string_of_literal lit)
  ;
  ^ "}"
and string_of_string_list = function
  | [] -> ""
  | str :: strlist ->
      strlit str ^ string_of_string_list_ strlist
and string_of_string_list_ = function
  | [] -> ""
  | x -> "," ^ string_of_string_list x
and string_of_param id t =
  "{" ^
  kind "Param" ^ "," ^
  prop "id" (strlit id) ^ "," ^
  prop "type" (strlit t)
  ^ "}"
and string_of_param_list = function
  | [] -> ""
  | (id, t) :: paramlist ->
      string_of_param id t ^ string_of_param_list_ paramlist
and string_of_param_list_ = function
  | [] -> ""
  | x -> "," ^ string_of_param_list x
and string_of_binop = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Mod -> "mod"
  | Equal -> "equal"
  | NotEqual -> "not equal"
  | LT -> "less than"
  | LTE -> "less than equal"
  | GT -> "greater than"
  | GTE -> "greater than equal"
and string_of_unaop = function
  | Not -> "not"
and string_of_literal lit =
  "{" ^
  match lit with
  | Null ->
      kind "Null"
  | Int (num) ->
      kind "Int" ^ "," ^
      prop "num" (string_of_int num)
  | Bool (b) ->
      kind "Bool" ^ "," ^
      prop "bool" (string_of_bool b)
  | String (str) ->
      kind "String" ^ "," ^
      prop "str" (strlit str)
  ;
  ^ "}"
;;



