(*
Assign0-4: 10 points
Please implement a function that converts a given
string to an integer:
fun str2int(cs: string): int
In particular, it is expected that str2int(int2str(x)) = x
for natural numbers x.
(You can assume that the given string is a sequence of digits)
(And the empty sequence represents the integer 0)
*)
let string_length = String.length
let string_get(cs, i0) = String.get cs i0
let length(str:string):int =
  string_length(str)
;;
let matchToInt(x:char): int =
  if x = '0' then 0
  else if x = '1' then 1
  else if x = '2' then 2
  else if x = '3' then 3
  else if x = '4' then 4
  else if x = '5' then 5
  else if x = '6' then 6
  else if x = '7' then 7
  else if x = '8' then 8
  else 9

let rec
indexparse(y:int): int =
  if y == 1 then 0
  else indexparse(y-1)
let rec
goesThrough(s:string): char = 
  if length(s) == 0 then string_get(s, indexparse(length(s)))
  else goesThrough(s)
    
;;
let str2int(cs: string): int = 
  matchToInt(goesThrough(cs))
;;
let test = str2int("133") 
;;