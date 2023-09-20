(* ************************************************ *)

(* Question 6: 10 points *)

let pp x y = fun f -> f(x,y)

let ff = 
   let sub(a:int)(b:int): int =
      a-b
in (ff(a)(b))

(* Give an implementation of ff that
   makes the following assertions pass *)

let () = assert (pp 200 100 ff = 100)
let () = assert (pp 100 200 ff = -100)

(* ************************************************ *)
