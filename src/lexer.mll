(* lexer.mll *)

{
open Ast
open Parser (* The token is defined in parser.mli *)
exception Eof

let get_pos lb =
  let sp = Lexing.lexeme_start_p lb in
  let ep = Lexing.lexeme_end_p lb in
  {
    start_p = {
      fname = sp.pos_fname;
      lnum = sp.pos_lnum;
      bol = sp.pos_bol;
      cnum = sp.pos_cnum
    };
    end_p = {
      fname = ep.pos_fname;
      lnum = ep.pos_lnum;
      bol = ep.pos_bol;
      cnum = ep.pos_cnum
    }
  }
;;

type keyword =
  | KEY_FUN
  | KEY_RET
  | KEY_VAR
;;

let keyword_table = Hashtbl.create 20
let _ =
  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
  [
    "fun", KEY_FUN;
    "ret", KEY_RET;
    "var", KEY_VAR
  ]
}

let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z']
let alnum = digit | alpha | '_'



rule token = parse
| [' ' '\t' '\n' '\r'] { token lexbuf } (* skip token *)
| ['0'-'9']+          { INT (get_pos lexbuf, int_of_string (Lexing.lexeme lexbuf)) }

| '+'                 { PLUS (get_pos lexbuf) }
| '-'                 { MINUS (get_pos lexbuf) }
| '*'                 { MUL (get_pos lexbuf) }
| '/'                 { DIV (get_pos lexbuf)}

| '('                 { PARENL (get_pos lexbuf)}
| ')'                 { PARENR (get_pos lexbuf)}

| '{'                 { BRACEL (get_pos lexbuf)}
| '}'                 { BRACER (get_pos lexbuf)}

| '['                 { BRACKETL (get_pos lexbuf)}
| ']'                 { BRACKETR (get_pos lexbuf)}

| alpha alnum*        {
let id = Lexing.lexeme lexbuf in
try
  match Hashtbl.find keyword_table id with
  | KEY_FUN -> FUN (get_pos lexbuf)
  | KEY_RET -> RET (get_pos lexbuf)
  | KEY_VAR -> VAR (get_pos lexbuf)
  ;
with Not_found ->
  IDENT (get_pos lexbuf, id)
}

| eof                 { EOF (get_pos lexbuf) }

