%{
open Ast
%}


/* literals */
%token <Ast.meta*int> INT
%token <Ast.meta*string> STR

/* identifier */
%token <Ast.meta*string> IDENT

/* operators */
%token <Ast.meta> PLUS MINUS MUL DIV MOD

/* assignment operators */
%token <Ast.meta> ASSIGN

/* comparison operators */
%token <Ast.meta> EQ NOTEQ LT LTE GT GTE NOT 

/* parenthesis, braces, brackets */
%token <Ast.meta> PARENL PARENR BRACEL BRACER BRACKETL BRACKETR

/* symbols */
%token <Ast.meta> COLON SEMICOLON CAMMA

/* keywords */
%token <Ast.meta> FUNC RET VAR FOR BREAK CONTINUE IF ELSE TRUE FALSE

/* others */
%token <Ast.meta> EOF

%left PLUS MINUS
%left MUL DIV MOD
%left EQ NOTEQ LT LTE GT GTE
%nonassoc UMINUS NOT

%start main
%type <Ast.program> main




%%

main:
  stmt_list EOF { { program=$1 } }
;

stmt:
  | expr SEMICOLON {
    Expression($1)
  }

  | BRACEL stmt_list BRACER {
    Block($2)
  }

  | VAR IDENT ASSIGN expr SEMICOLON {
    let (_, id) = $2 in VarDecl (id, $4)
  }

  | FOR expr SEMICOLON expr SEMICOLON expr BRACEL stmt_list BRACER
  {
    For($2, $4, $6, $8)
  }
  | BREAK SEMICOLON { Break }
  | CONTINUE SEMICOLON { Continue }

  | IF expr BRACEL stmt_list BRACER {
    If ($2, $4, [])
  }
  | IF expr BRACEL stmt_list BRACER ELSE BRACEL stmt_list BRACER
  {
    If ($2, $4, $8)
  }

  | FUNC IDENT PARENL param_list PARENR IDENT BRACEL stmt_list BRACER {
    let (_, id) = $2 in
    let (_, tstr) = $6 in
    Func(id, { typestr=tstr }, $4, $8)
  }

  | RET expr SEMICOLON {
    Ret ($2)
  }
;

expr:
  | INT   {
    let (_, num) = $1 in
    Literal(Int(num))
  }
  | STR {
    let (_, str) = $1 in Literal (String (str))
  }
  | TRUE { Literal (Bool (true)) }
  | FALSE { Literal (Bool (false)) }

  | PARENL expr PARENR {
    $2
  }

  | expr PLUS expr {
    Binary(Add, $1, $3)
  }
  | expr MINUS expr {
    Binary(Sub, $1, $3)
  }
  | expr MUL expr {
    Binary(Mul, $1, $3)
  }
  | expr DIV expr {
    Binary(Div, $1, $3)
  }
  | expr MOD expr {
    Binary(Mod, $1, $3)
  }
  | MINUS expr %prec UMINUS {
    Binary(Mul, $2, Literal(Int(-1)))
  }

  | expr EQ expr {
    Binary (Equal, $1, $3)
  }
  | expr NOTEQ expr {
    Binary (NotEqual, $1, $3)
  }
  | expr LT expr {
    Binary (LT, $1, $3)
  }
  | expr LTE expr {
    Binary (LTE, $1, $3)
  }
  | expr GT expr {
    Binary (GT, $1, $3)
  }
  | expr GTE expr {
    Binary (GTE, $1, $3)
  }
  | NOT expr {
    Unary (Not, $2)
  }

  | expr PARENL expr_list PARENR {
    Call ($1, $3)
  }

  | IDENT ASSIGN expr {
    let (_, id) = $1 in Assign (id, $3)
  }

  | IDENT {
    let (_, id) = $1 in Ident(id)
  }
;

stmt_list:
  | stmt { [$1] }
  | stmt stmt_list { $1 :: $2 }
;

expr_list:
  | /* empty */ { [] }
  | expr { [$1] }
  | expr CAMMA expr_list { $1 :: $3 }
;

param_list:
  | /* empty */ { [] }
  | IDENT IDENT {
    let (_, id) = $1 in
    let (_, t) = $2 in
    [ (id, { typestr=t }) ]
  }
  | IDENT IDENT CAMMA param_list {
    let (_, id) = $1 in
    let (_, t) = $2 in
    (id, { typestr=t }) :: $4
  }
;

ident_list:
  | /* empty */ { [] }
  | IDENT { let (_, id) = $1 in [id] }
  | IDENT CAMMA ident_list {
    let (_, id) = $1 in
    id :: $3
  }
;

