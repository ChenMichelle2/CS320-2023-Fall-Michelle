(*
Assign3-1:
HX-2023-09-26: 10 points
A matrix can be represented as a list of lists.
For instance, [[1;2;3];[4;5;6];[7;8;9]] represents
the following 3x3 square matrix:
1 2 3
4 5 6
7 8 9
Please implement matrix_transpose that returns
the transpose of a given matrix:
let rec
matrix_transpose(xss: 'a list list): 'a list list
For instance, the transpose of the above matrix
is given as follows:
1 4 7
2 5 8
3 6 9
You are allowed to define recursive functions to
solve this problem.
*)
let rec map_list f list =
  match list with
  | [] -> []
  | head :: tail -> (f head) :: (map_list f tail)

let hd lst =
  match lst with
  | [] -> failwith "empty list"
  | hd :: _ -> hd

let tl lst =
  match lst with
  | [] -> failwith "custom_tl: empty list"
  | _ :: tl -> tl
let rec
matrix_transpose(xss: 'a list list): 'a list list =
let rec get_column matrix =
  match matrix with
  | [] -> []
  | [] :: _ -> []
  | _ -> map_list hd matrix :: get_column (map_list tl matrix)
in
match xss with
| [] -> []
| _ -> get_column xss



