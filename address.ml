
(* corebuild  -package zarith,sha,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax address.byte *)


let encode_base58 (value: Z.t) =
  let code_string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
  let rec f div acc =
    if Z.gt div Z.zero then
      let div1, rem = Z.div_rem div (Z.of_int 58) in
      f div1 (code_string.[Z.to_int rem]::acc)
    else
      acc
  in
  Core.Std.String.of_char_list (f value [])

(* we only need the Z thing for the division/remainder action
  and we might be able to implement it ourselves.  otherwise we need
  to write conversion actions.
  

  we've already got the sha stuff, but we need hex_to_binary to be able
  to work a bit more easily. 
*)

let f x = 
  (* straight pattern match might be simpler/faster *)
  if x >= int_of_char '0' && x <= int_of_char '9' then
    x - (int_of_char '0') 
  else if x >= int_of_char 'a' && x <= int_of_char 'f' then
    x - (int_of_char 'a') + 10 
  else
    12345
 

(* TODO change name hex_of_binary *)
let string_of_hex s =
  let n = String.length s in
  let buf = Buffer.create (n/2) in
  for i = 0 to n/2-1 do
    let i2 = i * 2 in
    let x = f (int_of_char s.[i2]) in
    let y = f (int_of_char s.[i2+1]) in
    (*
    let () = Printf.printf "x %d, y %d\n" x y in
    let () = Printf.printf "here %d\n" ( x lsl 4 + y)  in
    *)
    (* let x, y = hex_of_char s.[i] in *)
    Buffer.add_char buf @@ char_of_int (x lsl 4 + y) ;
  done;
  Buffer.contents buf


let x = "c1a235aafbb6fa1e954a68b872d19611da0c7dc9" in 
let () = Printf.printf "original string %s\n" x in
let y = string_of_hex x in 
let () = Printf.printf "hex_of_string   %s\n" (Message.hex_of_string y) in
let z = Z.of_bits (Message.strrev y) in

(*
let x = Z.of_string_base 16 "c1a235aafbb6fa1e954a68b872d19611da0c7dc9" in
*)

Printf.printf "z is %s\n" (Z.format "x" z) 

(*
let s = Z.to_bits x in
let ds = Message.hex_of_string (Message.strrev s) in 
let () = Printf.printf "ds is %s\n" ds in
let result = encode_base58 x  in
Printf.printf "whoot %s\n" result
*)


(*let result2 = Core.Std.String.of_char_list result in *)
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



