
(* corebuild  -package zarith,sha,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax address.byte *)

(* we only need the Z thing for the division/remainder action
  and we might be able to implement it ourselves.  otherwise we need
  to write conversion actions.

  we've already got the sha stuff, but we need hex_to_binary to be able
  to work a bit more easily.
*)


(* - this function probably ought to take a string as input
to encapsulate all teh Z handling internally
  - and the list concat is horrible
*)
let encode_base58 (value: string ) =
  let value = Z.of_bits (Core.Core_string.rev value) in
  let code_string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
  let rec f div acc =
    if Z.gt div Z.zero then
      let div1, rem = Z.div_rem div (Z.of_int 58) in
      f div1 (code_string.[Z.to_int rem]::acc)
    else
      acc
  in
  Core.Std.String.of_char_list (f value [])


let int_of_hex (c : char) =
  (* change name int_of_hex_char ? *)
  (* straight pattern match might be simpler/faster *)
  let c1 = int_of_char c in
  if c >= '0' && c <= '9' then
    c1 - (int_of_char '0')
  else
    10 + c1 - (int_of_char 'a')


(* TODO change name hex_of_binary *)
let string_of_hex s =
  let n = String.length s in
  let buf = Buffer.create (n/2) in
  for i = 0 to n/2-1 do
    let i2 = i * 2 in
    let x = int_of_hex s.[i2] in
    let y = int_of_hex s.[i2+1] in
    Buffer.add_char buf @@ char_of_int (x lsl 4 + y)
  done;
  Buffer.contents buf

let encode_bitcoin_address (x: string) =
  let y = "\x00" ^ x in
  let checksum = Message.checksum2 y in
  let result = encode_base58 (y ^ checksum) in
  result



let s = "c1a235aafbb6fa1e954a68b872d19611da0c7dc9" in
let () = Printf.printf "string %s\n" s in
let s1 = string_of_hex s in
Printf.printf "z is %s\n" @@ encode_bitcoin_address s1



(*
let x = Z.of_string_base 16 "c1a235aafbb6fa1e954a68b872d19611da0c7dc9" in
*)


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



