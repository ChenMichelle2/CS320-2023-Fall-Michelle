#use "MyOCaml.ml";;

let sexpr_to_string (e : sexpr)  : string       = 
  string_make_fwork(fun work -> list_foreach e work)

let sexpr_parse     (s : string) : sexpr option = 
  match string_parse (parse_expr ()) s with
    | Some (e, []) -> Some e
    | _ -> None

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