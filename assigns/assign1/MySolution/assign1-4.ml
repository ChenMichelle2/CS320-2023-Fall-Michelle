(*
assign1-4: 20 points
Given two strings ds1 and ds2 of digits, please
implement intrep_add(ds1)(ds2) that returns the
string representing the sum of the two integers
represented by ds1 and ds2. Note that the returned
string should contain no leading zeros. (And we
use the empty string to represent the number zero).

fun
intrep_add(ds1: string)(ds2: string): string

For instance, intrep_add("1116123")("222987") = "1339110"

Note that ds1 and ds2 can be arbitrarily long. Thus,
converting ds1 and ds2 to integers can cause overflow.
*)
#use "./../../../classlib/OCaml/MyOCaml.ml";;
let intrep_add(ds1: string)(ds2: string): string =
  let len1 = string_length(ds1) and len2 = string_length(ds2) 
    in let getChar1(i) = 
      if i >= len1 then 0
      else digit_of_char(string_get_at(ds1)(len1-1-i)) and
      getChar2(i) = 
        if i >= len2 then 0
        else digit_of_char(string_get_at(ds2)(len2-1-i)) 
      in string_rmake_fwork(
        fun work -> let first = int1_foldleft(max(len1)(len2))(0)
        (fun a0 i -> 
          let a1 = a0 + getChar1(i) + getChar2(i) 
            in (work(char_of_digit(a1 mod 10)); a1/10))
        in 
          if first > 0 then work(char_of_digit(first))
      )
    ;;


