
(**
assign1-1: 10 points
Given a natural number n that is not a multiple
of 10, intrev10(n) is the natural number whose
representation in base 10 reverses that of the
number n.

fun intrev10(n: int): int

For instance, if n = 12345, then intrev10(n) is
54321; if n = 10203, then intrev10(n) is 30201.

Please give a TAIL-RECURSIVE implementation of
intrev10.
*)

let intrev10(n: int): int=
  let rec singleInt(x:int)(y:int): int =
    if x < 10 then (y*10)+x
    else singleInt(x/10)((y*10)+(x mod 10))
  in singleInt(n)(0) 
;;
let test =  intrev10(12345);;