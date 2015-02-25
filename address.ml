
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

  should we do more core hoisting - eg. zero and value of 58 here?
*)

let encode_base58 value =
  let code_string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
  let rec f div acc =
    if Z.gt div Z.zero then
      let div1, rem = Z.div_rem div (Z.of_int 58) in
      f div1 (code_string.[Z.to_int rem]::acc)
    else
      acc
  in
  Core.Std.String.of_char_list (f value [])

(* ok if it's a hex string how do we convert back to a string buffer??? 
    It's easy to tack on "\x0\x0" to the front.
  it would be better

  This cannot be done, with a numerical interpretation. so we have to use the
  buffer.

  string |> hex_of_string |> Z.of_string_base 16 (which isn't great)

  string_of_bighex ...   we want this. 
  
  should be easy though... it's just scanning the bytes, and setting them.
  unless Z can already do it.

  Ok, we should be able to load a bin string just with left shifts ok.
 
  Ok, so we can convert between hex string representations
  but not binary representations.
  - ok, there are functions to_bits and of_bits ... which are guaranteed little endian 

  - it's byte ordered... 
*)

let x = Z.of_string_base 16 "c1a235aafbb6fa1e954a68b872d19611da0c7dc9" in

let () = Printf.printf "y is %s\n" (Z.format "x" x) in

let s = Z.to_bits x in

let ds = Message.hex_of_string (Message.strrev s) in 
 
let () = Printf.printf "ds is %s\n" ds in

let result = encode_base58 x  in
(*let result2 = Core.Std.String.of_char_list result in *)

Printf.printf "whoot %s\n" result

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



