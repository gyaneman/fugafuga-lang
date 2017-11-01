(* main.ml *)

let exec_file fname =
  let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let result = Parser.main Lexer.token lexbuf in
    close_in ic;
    print_int result;
    print_newline();
    flush stdout;
  with e ->
    close_in_noerr ic;
    raise e;
;;

let usagemsg = "./main.byte program.fgl"

let main =
  let speclist = [] in
  Arg.parse speclist exec_file usagemsg
;;

main

