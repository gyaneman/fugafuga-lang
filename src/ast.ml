
type pos = { fname: string; lnum: int; bol: int; cnum: int };;
type meta = { start_p: pos; end_p: pos };;

let string_of_pos { fname = fn; lnum = ln; bol = bo; cnum = cn } =
  "(fname:" ^ fn ^ ", lnum:" ^ string_of_int ln ^
  ", bol:" ^ string_of_int bo ^ ", cnum:" ^ string_of_int cn ^ ")"
;;
let string_of_meta { start_p = sp; end_p = ep } =
  "(start_p:" ^ string_of_pos sp ^ ", end_p:" ^ string_of_pos ep ^ ")"
;;
  
