(* ****** ****** *)



(* ****** ****** *)
let chr = Char.chr
let ord = Char.code
let string_init = String.init
let len = 0
let int2str(i0: int): string =
  if i0 < 10 then string_init 1 (fun i0 -> chr(ord('0') + i0))
  else string_init (i0 mod 10) (fun i0 -> chr(ord('0') + i0))

(*helper function to determine the length of the int*)
let lenOfInt(i0:int): int =
  if i0 < 10 then 1 else i0 mod 10;;
  