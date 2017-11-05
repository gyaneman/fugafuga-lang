(* main.ml *)

open Ast
open Value
open Environment


let exec_file fname =
  let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let result = Parser.main Lexer.token lexbuf in
    close_in ic;
    print_string (string_of_statement result);
    print_newline();
    flush stdout;
  with e ->
    close_in_noerr ic;
    raise e;
;;

let usagemsg = "./main.byte program.fgl"

let main =
  let env = extend_env "zero" (IntVal(0)) [] in
  print_string "init env\n";
  print_env env;
  print_string "\n\n";
  let speclist = [] in
  Arg.parse speclist exec_file usagemsg
;;

main

