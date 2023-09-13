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

let rec
divideBy10(x:int): int=
  if x < 10 then 1
  else 1 + divideBy10(x/10)
;;

let 
matchtostring(y:int): string =
  if y == 0 then "0"
  else if y == 1 then "1"
  else if y == 2 then "2"
  else if y == 3 then "3"
  else if y == 4 then "4"
  else if y == 5 then "5"
  else if y == 6 then "6"
  else if y == 7 then "7"
  else if y == 8 then "8"
  else "9"
;;

let rec
parseThru(z:int): string =
  if z < 10 then matchtostring(z mod 10)
  (*need to write my own concatenation, use string_init *)
  else parseThru(z/10)^matchtostring(z mod 10)
;;

let int2str(i0: int): string =
  if i0<>0 then parseThru(i0)
  else "0"
;;

let test = int2str(133) 
;; 

