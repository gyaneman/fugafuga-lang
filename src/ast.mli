
type pos = { fname: string; lnum: int; bol: int; cnum: int }
type meta = { start_p: pos; end_p: pos }

val string_of_pos : pos -> string;;
val string_of_meta : meta -> string;;

