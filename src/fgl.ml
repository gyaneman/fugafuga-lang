(*
 * fgl.ml
 * This is the top level file.
 *)

open Ast
open Value
open Environment
open Interp

let exec_file fname =
  let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let ast = Parser.main Lexer.token lexbuf in
    close_in ic;
    print_string (string_of_program ast);
    print_newline();
    let result = interp ast [] in print_value result;
    flush stdout;
  with e ->
    close_in_noerr ic;
    raise e;
;;

let usagemsg = "./fgl.byte program.fgl"

let main =
  let speclist = [] in
  Arg.parse speclist exec_file usagemsg
;;

main

