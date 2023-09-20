(* ************************************************ *)

(*

 Question 8: 20 points
 Please give a NON-RECURSIVE implementation of sort5
 that takes 5 integers and returns a tuple that consists
 exactly of the 5 given integers ordered increasingly

 For instance, sort5(1, 2, 1, 2, 1) = (1, 1, 1, 2, 2)
 For instance, sort5(1, 3, 4, 5, 2) = (1, 2, 3, 4, 5)
 For instance, sort5(1, 3, 5, 4, 2) = (1, 2, 3, 4, 5)

 You can implement your own helper functions as long as
 you do not make use of recursion.

*)
let smallest (h:int)(i:int)(j:int)(k:int)(l:int): int =
  if (h <= l && h <= i && h <= j && h <= k) then h
  else if (i <= l && i <= h && i <= j && i <= k) then i
  else if (j <= l && j <= i && j <= h && j <= k) then j
  else if (k <= l && k <= i && k <= j && k <= h) then k
  else l
let sort5: int*int*int*int*int -> int*int*int*int*int =
  (smallest(sort5) 


(* ************************************************ *)
