#use "./../../../classlib/OCaml/MyOCaml.ml";;
(* No other library imports are allowed. *)

(* ************************************************ *)

(* Question 7: 10 points

   Given the following snippet, implement the test
   function so that isPrime returns true for prime
   number inputs and false otherwise. *)

let isPrime(n) =
  let test(i:int): bool = 
    if factors(n0)(n0-1) then false else if (n0 == 0) then false 
    else true
    let rec
    factors(x:int)(y:int): bool=
      if y >= 2 then (x mod y == 0 || factors(x)(y-1))else false
  in
  if n < 2 then false else int1_forall(n)(test)
;;
(* ************************************************ *)
