#use "MyOCaml.ml";;
type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *)

let sexpr_to_string (e : sexpr)  : string = 
  match stdin with
    | SInt x -> intToString(x)
    | SAdd lst -> "(add " + sexpr_to_string(list_foreach(e)) + ")"
    | SMul lst2 -> "(mul " 
  

let rec parse_sexpr () : sexpr parser =
  sexpr_int () <|> sexpr_add () <|> sexpr_mul ()

and sexpr_int () : sexpr parser =
  let* n = natural in
  pure (Int n) << whitespaces
  
and sexpr_add () : sexpr parser =
  let* _ = keyword "(add" in
  let* es = many1' parse_sexpr in
  let* _ = keyword ")" in
  pure (Add es)

and sexpr_mul () : sexpr parser =
  let* _ = keyword "(mul" in
  let* es = many1' parse_sexpr in
  let* _ = keyword ")" in
  pure (Mul es)

let sexpr_parse  (s : string) : sexpr option = 
match sexpr_to_string (parse_sexpr ()) s with
  | Some (e, []) -> Some e
  | _ -> None
