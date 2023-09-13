(* ****** ****** *)



(* ****** ****** *)
let chr = Char.chr
let ord = Char.code
let string_init = String.init
let int2str(i0: int): string =
  string_init i0 (fun i0 -> chr(ord('0') + i0));;
