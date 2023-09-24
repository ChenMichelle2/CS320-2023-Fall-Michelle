(*
assign1-3: 10 points
A 3-letter sequence abc is 132-like
if a < c < b holds. For instance, 123 is
not 132-like; but 596 is 132-like.

A string is 132-avoid if there is no subsequence
abc in this string that is 132-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 132-avoid;
For instance, 987654321 is 132-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 165 is 132-like.

Please implement a function string_avoid_132 that
checks if a given string is 132-avoid; the function
returns true if and only if the given string is 132-
avoid.

fun string_avoid_132(cs: string): bool
*)
#use "./../../../classlib/OCaml/MyOCaml.ml";;
let string_avoid_132(cs: string): bool =
  let len = string_length(cs) in 
    let rec
      triple1(i: int): bool = 
        int1_forall(len-i-1)(fun a -> triple2(i)(i+a+1)) 
      and
      triple2(i:int)(a:int): bool = 
        int1_forall(len-a-1)(fun b -> triple3(i)(a)(a+b+1))
      and
      triple3(i:int)(a:int)(b:int): bool = 
        let chari = string_get_at(cs)(i)
        and
        chara = string_get_at(cs)(a)
        and
        charb = string_get_at(cs)(b)
        in not ((chari < charb) && (charb < chara))
      in 
    int1_forall(len)(fun i -> triple1(i))
    ;;