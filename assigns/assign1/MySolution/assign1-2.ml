(*
assign1-2: 10 points
Given two ordered strings cs1 and cs2, please use
string_make_fwork to implement string_merge(cs1, cs2)
which returns the order string obtained from merging
cs1 and cs2.

For instance, if cs1 = "135" and cs2 = "2468", then
string_merge(cs1)(cs2) equals "1234568"

For instance, if cs1 = "abcde" and cs2 = "1234", then
string_merge(cs1)(cs2) equals "1234abcde"
*)
#use "./../../../classlib/OCaml/MyOCaml.ml";;
let string_merge(cs1:string)(cs2:string): string=
  let lenCs1 = string_length(cs1) and lenCs2 = string_length(cs2) in 
  let rec
    foreach(x: int)(y: int)(work) = 
    if x < lenCs1 then ( 
      if y < lenCs2 then
        let charCs1 = string_get_at(cs1)(x) and charCs2 = string_get_at(cs2)(y) in
          if charCs1 <= charCs2 
            then (work(charCs1); foreach(x+1)(y+0)(work))
          else (work(charCs2); foreach(x+0)(y+1)(work))
        
      else int1_foreach(lenCs1-x)
          (fun i -> work(string_get_at(cs1)(x+i)))
      )
    else int1_foreach(lenCs2-y)
      (fun i -> work(string_get_at(cs2)(y+i)))
    
    in
      string_make_fwork(foreach(0)(0))
;;
(*let test = string_merge("abc")("123");;*)
