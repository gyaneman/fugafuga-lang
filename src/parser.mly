/* parser.mly */

%token <int> INT
%token PLUS MINUS MUL DIV
%token PARENL PARENR BRACEL BRACER BRACKETL BRACKETR
%token EOF

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
  | INT   { $1 }
  | PARENL expr PARENR { $2 }
  | expr PLUS expr { $1 + $3 }
  | expr MINUS expr { $1 - $3 }
  | expr MUL expr { $1 * $3 }
  | expr DIV expr { $1 * $3 }
  | MINUS expr %prec UMINUS { - $2 }
;


