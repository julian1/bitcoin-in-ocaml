
(* corebuild  -package zarith,sha,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax address.byte *) 

(*
   code_string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
   x = convert_bytes_to_big_integer(hash_result)
   output_string = ""
   while(x > 0) 
       {
           (x, remainder) = divide(x, 58)
           output_string.append(code_string[remainder])
       }
let s = Z.to_string u in
Printf.printf "%s\n" s

*)

let code_string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in


let u = Z.of_int 58 in
let div  = Z.of_string "0xc1a235aafbb6fa1e954a68b872d19611da0c7dc9"  in

let div, rem = Z.div_rem div u in 
let rem_ = code_string.[ Z.to_int rem] in
let () = Printf.printf "whoot %c\n" rem_  in

let div, rem = Z.div_rem div u in 
let rem_ = code_string.[ Z.to_int rem] in
Printf.printf "whoot %c\n" rem_ 




