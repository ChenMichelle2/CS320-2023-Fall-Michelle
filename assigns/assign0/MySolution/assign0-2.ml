(* ****** ****** *)

#use "./../assign0.ml";;
(* ****** ****** *)
let rec
factors(x:int)(y:int): bool=
  if y >= 2 then (x mod y == 0 || factors(x)(y-1))else false;;

let 
isPrime(n0: int): bool =
  if factors(n0)(n0-1) then false else if (n0 == 0) then false else true;;


    