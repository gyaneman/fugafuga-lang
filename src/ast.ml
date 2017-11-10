(* ast.ml *)

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

(* expressions *)
type expression =
  | Binary of bin_op * expression * expression
  | Unary of una_op * expression
  | Assign of string * expression (* dst, src *)
  | Call of expression * expression list
  | Ident of string
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
  | Func of string * string list * statement list
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

let rec string_of_program pg =
  "{" ^
  prop "type" (strlit "Program") ^ "," ^
  prop "program" ("[" ^ string_of_statement_list pg.program ^ "]")
  ^ "}"
and string_of_statement statement = 
  "{" ^
  match statement with
  | Block (stmtlist) ->
      prop "type" (strlit "Block") ^ "," ^
      prop "statement_list" ("[" ^ string_of_statement_list stmtlist ^ "]")
  | For (init, cond, update, body) ->
      prop "type" (strlit "For") ^ "," ^
      prop "init" (string_of_expression init) ^ "," ^
      prop "cond" (string_of_expression cond) ^ "," ^
      prop "update" (string_of_expression update) ^ "," ^
      prop "body" ("[" ^ string_of_statement_list body ^ "]")
  | Break ->
      prop "type" (strlit "Break")
  | Continue ->
      prop "type" (strlit "Continue")
  | If (exp, conseq, alter) ->
      prop "type" (strlit "If") ^ "," ^
      prop "condition" (string_of_expression exp) ^ "," ^
      prop "conseq" ("[" ^ string_of_statement_list conseq ^ "]") ^ "," ^
      prop "alter" ("[" ^ string_of_statement_list alter ^ "]")
  | Expression (exp) ->
      prop "type" (strlit "Expression") ^ "," ^
      prop "exp" (string_of_expression exp)
  | VarDecl (dst_id, src_exp) ->
      prop "type" (strlit "VarDecl") ^ "," ^
      prop "destination" (strlit dst_id) ^ "," ^
      prop "source" (string_of_expression src_exp)
  | Func (id, params, stmts) ->
      prop "type" (strlit "Func") ^ "," ^
      prop "id" (strlit id) ^ "," ^
      prop "params" ("[" ^ string_of_string_list params ^ "]") ^ "," ^
      prop "stmts" ("[" ^ string_of_statement_list stmts ^ "]")
  | Ret (exp) ->
      prop "type" (strlit "Ret") ^ "," ^
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
      prop "type" (strlit "BinaryExp") ^ "," ^
      prop "operator" (strlit (string_of_binop op)) ^ "," ^
      prop "left" (string_of_expression left) ^ "," ^
      prop "right" (string_of_expression right)
  | Unary (op, e) ->
      prop "type" (strlit "UnaryExp") ^ "," ^
      prop "operator" (strlit (string_of_unaop op)) ^ "," ^
      prop "exp" (string_of_expression e)
  | Assign (dst, src) ->
      prop "type" (strlit "Assign") ^ "," ^
      prop "destination" (strlit dst) ^ "," ^
      prop "source" (string_of_expression src)
  | Call (f, args) ->
      prop "type" (strlit "Call") ^ "," ^
      prop "f" (string_of_expression f) ^ "," ^
      prop "args" ("[" ^ string_of_expression_list args ^ "]")
  | Ident (id) ->
      prop "type" (strlit "Ident") ^ "," ^
      prop "id" (strlit id)
  | Literal (lit) ->
      prop "type" (strlit "LiteralExp") ^ ", " ^
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
      prop "type" (strlit "Null")
  | Int (num) ->
      prop "type" (strlit "Int") ^ "," ^
      prop "num" (string_of_int num)
  | Bool (b) ->
      prop "type" (strlit "Bool") ^ "," ^
      prop "bool" (string_of_bool b)
  | String (str) ->
      prop "type" (strlit "String") ^ "," ^
      prop "str" (strlit str)
  ;
  ^ "}"
;;



