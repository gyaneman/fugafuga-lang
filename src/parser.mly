%{
open Ast
%}


/* literals */
%token <Ast.meta*int> INT

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
%token <Ast.meta> SEMICOLON

/* keywords */
%token <Ast.meta> FUN RET VAR FOR IF ELSE

/* others */
%token <Ast.meta> EOF

%left PLUS MINUS
%left MUL DIV MOD
%left EQ NOTEQ LT LTE GT GTE
%nonassoc UMINUS NOT

%start main
%type <Ast.statement> main




%%

main:
  stmt_list EOF { Block($1) }
;

stmt:
  | expr SEMICOLON {
    Expression($1)
  }

  | BRACEL stmt_list BRACER {
    Block($2)
  }

  | FOR expr SEMICOLON expr SEMICOLON expr BRACEL stmt_list BRACER
  {
    For($2, $4, $6, Block($8))
  }

  | IF expr BRACEL stmt_list BRACER {
    If ($2, Block($4), Block([]))
  }
  | IF expr BRACEL stmt_list BRACER ELSE BRACEL stmt_list BRACER
  {
    If ($2, Block($4), Block($8))
  }
;

expr:
  | INT   {
    let (_, num) = $1 in
    Literal(Int(num))
  }

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

  | expr ASSIGN expr {
    Assign ($1, $3)
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

    | IDENT {
    let (_, id) = $1 in Ident(id)
  }
;

stmt_list:
  | stmt { [$1] }
  | stmt stmt_list { $1 :: $2 }
;


