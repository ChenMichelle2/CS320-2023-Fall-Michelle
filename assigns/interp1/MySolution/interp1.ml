#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)
type stack_op =
  | Push of int
  | Add
  | Sub
  | Trace
  | Stack
  | Program
  | Reduction

type program = command list

type stack = value list
type trace = string list
type config = stack * trace * program

let apply_op (stack : int list) (op : stack_op) : int list =
  match op with
  | Push x -> x :: stack
  | Add ->
    (match stack with
    | y :: x :: rest -> (x + y) :: rest
    )
  | Sub ->
    (match stack with
    | y :: x :: rest -> (x - y) :: rest
    )


let parse_program (program : string) : stack_op list =
  let parse_op s =
    match s with
    | "Add" -> Add
    | "Sub" -> Sub
      try Push (int_of_string s)
   with
   | Failure msg -> None
  in
  List.map parse_op (String.split_on_char ' ' program)

let interp (s : string) : string list option =
  try
    let program = parse_program s in
    let rec eval_stack (stack : int list) (ops : stack_op list) : int list =
      match ops with
      | [] -> stack
      | op :: rest -> eval_stack (apply_op stack op) rest
    in
    let result_stack = eval_stack [] program in
    Some (List.map string_of_int (List.rev result_stack))
  with
  | Failure msg -> None

