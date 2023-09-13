(*
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)
(* ****** ****** *)
let chr = Char.chr
let ord = Char.code
let string_init = String.init
let len = 0
let int2str(i0: int): string =
  if i0 < 10 then string_init 1 (fun i -> chr(ord('0') + i0))
  else string_init (i0 mod 10) (fun i -> chr(ord('0') + i0))
;;