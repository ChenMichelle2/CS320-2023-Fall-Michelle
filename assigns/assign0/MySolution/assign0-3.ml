(* ****** ****** *)



(* ****** ****** *)
let chr = Char.chr
let ord = Char.code
let string_init = String.init
let int2str(i0: int): string =
  string_init 2  (fun i0 -> chr(ord('a') + i0));;
  