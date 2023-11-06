#use "./../../../../classlib/OCaml/MyOCaml.ml";;


let explode (s: string) : char list = 
  let rec expl i l = 
    if i < 0 then l 
    else expl (i - 1) (String.get s i :: l) in 
  expl (String.length s - 1) []

let parse_expr (s:char list): expr option =
  match s with
  |
let parse (s : string) : expr option = 
  parse_expr (explode s)