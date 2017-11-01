%{
open Ast
%}


/* literals */
%token <Ast.meta*int> INT

/* identifier */
%token <Ast.meta*string> IDENT

/* operators */
%token <Ast.meta> PLUS MINUS MUL DIV

/* parenthesis, braces, brackets */
%token <Ast.meta> PARENL PARENR BRACEL BRACER BRACKETL BRACKETR

/* keywords */
%token <Ast.meta> FUN RET VAR

/* others */
%token <Ast.meta> EOF

%left PLUS MINUS
%left MUL DIV
%nonassoc UMINUS

%start main
%type <int> main




%%

main:
  expr EOF { $1 }
;

expr:
  | INT   { let (_, num) = $1 in num }
  | PARENL expr PARENR { $2 }
  | expr PLUS expr { $1 + $3 }
  | expr MINUS expr { $1 - $3 }
  | expr MUL expr { $1 * $3 }
  | expr DIV expr { $1 * $3 }
  | MINUS expr %prec UMINUS { - $2 }
;


