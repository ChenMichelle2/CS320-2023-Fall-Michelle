#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)
(* abstract syntax tree of interp2 *)
type const =
  | Int of int
  | Bool of bool
  | Unit
  | Sym of char

type com =
  | Push of const | Pop | Swap | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt
  | If of coms else of coms end
  | Bind | Lookup
  | Fun of coms end | Call | Return

(* parsers for interp2 *)
let parse_nat = 
   let* n = natural << whitespaces in pure n
 
 let parse_int =
   (let* n = parse_nat in pure (Int n)) <|>
   (keyword "-" >> let* n = parse_nat in pure (Int (-n)))
 
 let parse_bool =
   (keyword "True" >> pure (Bool true)) <|>
   (keyword "False" >> pure (Bool false))
 
 let parse_unit =
   keyword "Unit" >> pure Unit 

let parse_char = 
   let* c = char in pure c

let parse_sym = 
   (let* c = parse_char in pure (char c)) <|>
   (let* n = parse_nat in pure(Int n))
 
 let parse_const =
   parse_int <|>
   parse_bool <|>
   parse_unit <|>
   parse_sym
 
 let parse_com = 
   (keyword "Push" >> parse_const >>= fun c -> pure (Push c)) <|>
   (keyword "Pop" >> pure Pop) <|>
   (keyword "Trace" >> pure Trace) <|>
   (keyword "Add" >> pure Add) <|>
   (keyword "Sub" >> pure Sub) <|>
   (keyword "Mul" >> pure Mul) <|>
   (keyword "Div" >> pure Div) <|>
   (keyword "And" >> pure And) <|>
   (keyword "Or" >> pure Or) <|>
   (keyword "Not" >> pure Not) <|>
   (keyword "Lt" >> pure Lt) <|>
   (keyword "Gt" >> pure Gt)
   
 
 let parse_coms = many (parse_com << keyword ";")
 

type coms = com list

let interp (s : string) : string list option = (* YOUR CODE *)
