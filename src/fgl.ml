(*
 * fgl.ml
 * This is the top level file.
 *)

open Parse_tree
open Ast
open Ast_constructor
open Type
open Value
open Environment
open Interp

let enable_only_ast_print = ref false

let exec_file fname =
  let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let pt = Parser.main Lexer.token lexbuf in
    close_in ic;
    if !enable_only_ast_print then
      print_string (string_of_program pt)
    else
      let result = interp pt [] in
      print_value result;
      make_program pt init_type_env
    ;
    flush stdout;
  with e ->
    close_in_noerr ic;
    raise e;
;;

let usagemsg = "./fgl.byte program.fgl"

let main =
  let speclist = [
    ("-a", Arg.Set enable_only_ast_print, "pretty printing AST")
    ] in
  Arg.parse speclist exec_file usagemsg
;;

main

