
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

it would be nice to be able to write this stuff...
with folds rather than recursive functions...

fold can wo
- actually it's not a fold over the data it's a fold while a condition is true
- and it needs the value and acc and boolean

*)

let code_string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"  
let _58 = Z.of_int 58 
let zero = Z.zero


let f div = 
	let div1, rem = Z.div_rem div _58 in
	div1, code_string.[ Z.to_int rem]

let rec ff div acc =
  if Z.gt div zero then
    let div, c = f div in
    ff div (c::acc) 
  else
    acc


let x = Z.of_string "0xc1a235aafbb6fa1e954a68b872d19611da0c7dc9"   
let result = ff x [] in
let result2 = Core.Std.String.of_char_list result in

Printf.printf "whoot %s\n" result2

(* rather than constructing a list we should be generating a string *)


(*
Printf.printf "whoot\n"
*)


(*

in

let () = Printf.printf "whoot %c\n" c in
let div, c = f div in 
Printf.printf "whoot %c\n" c 

*)



