#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(* Please implement a parse function. When given a valid string according
   to the grammar, your parse function returns an expr value encoding the
   expression.

   Example (Accpeted Strings):
   parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
   parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])

   Example (Rejected Strings):
   parse "()" = None
   parse "(add)" = None
   parse "(add 1 2))" = None
   parse "((mul 1 2)" = None

*)
type expr =
  | Int of int       (* 1, 2, 3, 4 ...  *)
  | Add of expr list (* (add e1 e2 ...) *)
  | Mul of expr list (* (mul e1 e2 ...) *)

let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs

let explode (s: string) : char list = 
  let rec expl i l = 
    if i < 0 then l 
    else expl (i - 1) (String.get s i :: l) in 
  expl (String.length s - 1) []

let rec parse_int(x:char) : expr option = 

let rec parse_expr(s:char list) : expr option =
  match s with
  | x::'add'::tl -> (match parse_int x, parse expr tl with
                    |Some (Digit a), Some e ->  Some (Add (a,e))
                    | _ -> None)
                      )
    | _ -> None

let parse (s : string) : expr option = 
  parse_expr (explode s)