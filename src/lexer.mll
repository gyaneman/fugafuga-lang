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
  | KEY_FUNC
  | KEY_RET
  | KEY_VAR
  | KEY_FOR
  | KEY_IF
  | KEY_ELSE
  | KEY_TRUE
  | KEY_FALSE
;;

let keyword_table = Hashtbl.create 20
let _ =
  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
  [
    "func", KEY_FUNC;
    "ret", KEY_RET;
    "var", KEY_VAR;
    "for", KEY_FOR;
    "if", KEY_IF;
    "else", KEY_ELSE;
    "true", KEY_TRUE;
    "false", KEY_FALSE
  ]
}

let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z']
let alnum = digit | alpha | '_'

let esc_seq_chars = '"' | 'n'
let single_esc_seq_chars = '\'' | esc_seq_chars
let single_esc_seq = '\\' single_esc_seq_chars
let double_esc_seq_chars = '"' | esc_seq_chars
let double_esc_seq = '\\' double_esc_seq_chars
let single_str_chars = digit | alpha | ' ' | single_esc_seq
let double_str_chars = digit | alpha | ' ' | double_esc_seq


rule token = parse
| [' ' '\t' '\n' '\r'] { token lexbuf } (* skip token *)
| ['0'-'9']+          { INT (get_pos lexbuf, int_of_string (Lexing.lexeme lexbuf)) }
| '\'' single_str_chars* '\'' | '"' double_str_chars* '"'
      { STR (get_pos lexbuf, Lexing.lexeme lexbuf) }

| '+'                 { PLUS (get_pos lexbuf) }
| '-'                 { MINUS (get_pos lexbuf) }
| '*'                 { MUL (get_pos lexbuf) }
| '/'                 { DIV (get_pos lexbuf)}
| '%'                 { MOD (get_pos lexbuf) }

| '='                 { ASSIGN (get_pos lexbuf) }

| "=="                { EQ (get_pos lexbuf) }
| "!="                { NOTEQ (get_pos lexbuf) }
| "<"                 { LT (get_pos lexbuf) }
| "<="                { LTE (get_pos lexbuf) }
| ">"                 { GT (get_pos lexbuf) }
| ">="                { GTE (get_pos lexbuf) }

| '!'                 { NOT (get_pos lexbuf) }

| '('                 { PARENL (get_pos lexbuf)}
| ')'                 { PARENR (get_pos lexbuf)}

| '{'                 { BRACEL (get_pos lexbuf)}
| '}'                 { BRACER (get_pos lexbuf)}

| '['                 { BRACKETL (get_pos lexbuf)}
| ']'                 { BRACKETR (get_pos lexbuf)}

| ';'                 { SEMICOLON (get_pos lexbuf) }
| ','                 { CAMMA (get_pos lexbuf) }

| alpha alnum*        {
let id = Lexing.lexeme lexbuf in
try
  match Hashtbl.find keyword_table id with
  | KEY_FUNC -> FUNC (get_pos lexbuf)
  | KEY_RET -> RET (get_pos lexbuf)
  | KEY_VAR -> VAR (get_pos lexbuf)
  | KEY_FOR -> FOR (get_pos lexbuf)
  | KEY_IF -> IF (get_pos lexbuf)
  | KEY_ELSE -> ELSE (get_pos lexbuf)
  | KEY_TRUE -> TRUE (get_pos lexbuf)
  | KEY_FALSE -> FALSE (get_pos lexbuf)
  ;
with Not_found ->
  IDENT (get_pos lexbuf, id)
}

| eof                 { EOF (get_pos lexbuf) }

