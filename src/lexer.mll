(* lexer.mll *)

{
open Parser (* The token is defined in parser.mli *)
exception Eof
}



rule token = parse
| [' ' '\t' '\n']     { token lexbuf } (* skip token *)
| ['0'-'9']+          { INT(int_of_string (Lexing.lexeme lexbuf)) }
| '+'                 { PLUS }
| '-'                 { MINUS }
| '*'                 { MUL }
| '/'                 { DIV }
| '('                 { PARENL }
| ')'                 { PARENR }
| '{'                 { BRACEL }
| '}'                 { BRACER }
| '['                 { BRACKETL }
| ']'                 { BRACKETR }
| eof                 { EOF }

