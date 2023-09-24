(*
assign1-5: 20 points

A sequence of chars is ascending if any char in
the sequence is less than or equal to the following
one (when the following one does exist).
Given a string cs, please implement a function
that find the longest ascending subsequence of [cs].
If there are more than one such sequences, the left
most one should be returned.

fun string_longest_ascend(xs: string): string

For instance, given "1324561111", the function
string_longest_ascend returns "13456"

For instance, given "1234561111", the function
string_longest_ascend returns "123456"

For instance, given "1234511111", the function
string_longest_ascend returns "111111".
*)
#use "./../../../classlib/OCaml/MyOCaml.ml";;
let string_longest_ascend(xs: string): string =
let rec
  helperfun(xs) =
    let len = string_length(xs) in
      if len <= 1 then xs 
      else(
        let h = string_head(xs) and i = string_tail(xs) in
          let j = helper(i) in
            if h <= string_head(j) then string_cons(h)(j) 
            else let i2 = string_filter i (fun x -> h <= x) in
            let j2 = helper(i2) in
              if string_length(j2) + 1 >= string_length(j) then string_cons(h)(j2) else j
      )
in
  helperfun(xs)
;; 

let test = string_longest_ascend ("1324561111");;