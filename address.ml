
(* corebuild  -package zarith,sha,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax address.byte *)

<<<<<<< HEAD
(* 
  what's next?
    - encode the public key... to show all tx inputs and outputs for pay to script. if bytes length is correct.
    - then do script engine.
*)


let encode_base58 (value: string ) =
  (* TODO maybe replace list concat with Buffer for speed. 
  - we only need the Z type for arbitrary precision division/remainder
  might be able to implement ourselves in base 256. *)
=======
(* - this function probably ought to take a string as input 
to encapsulate all teh Z handling internally 
  - and the list concat is horrible
*)
let encode_base58 (value: Z.t) =
>>>>>>> devel
  let code_string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
  let rec f div acc =
    if Z.gt div Z.zero then
      let div1, rem = Z.div_rem div (Z.of_int 58) in
      f div1 (code_string.[Z.to_int rem]::acc)
    else
      acc
  in
<<<<<<< HEAD
  let value = Z.of_bits (Core.Core_string.rev value) in
  Core.Std.String.of_char_list (f value [])


let int_of_hex (c : char) =
  (* change name int_of_hex_char ? *)
  (* straight pattern match might be simpler/faster *)
  let c1 = int_of_char c in
  if c >= '0' && c <= '9' then
    c1 - (int_of_char '0')
  else
    10 + c1 - (int_of_char 'a')


let string_of_hex (s: string) =
  (* TODO perhaps rename binary_of_hex *)
=======
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
>>>>>>> devel
  let n = String.length s in
  let buf = Buffer.create (n/2) in
  for i = 0 to n/2-1 do
    let i2 = i * 2 in
<<<<<<< HEAD
    let x = int_of_hex s.[i2] in
    let y = int_of_hex s.[i2+1] in
    Buffer.add_char buf @@ char_of_int (x lsl 4 + y)
=======
    let x = f (int_of_char s.[i2]) in
    let y = f (int_of_char s.[i2+1]) in
    (*
    let () = Printf.printf "x %d, y %d\n" x y in
    let () = Printf.printf "here %d\n" ( x lsl 4 + y)  in
    *)
    Buffer.add_char buf @@ char_of_int (x lsl 4 + y) ;
>>>>>>> devel
  done;
  Buffer.contents buf


<<<<<<< HEAD
let btc_address_of_hash160 (a: string) =
  let x = "\x00" ^ a in
  let checksum = Message.checksum2 x in
  encode_base58 (x ^ checksum)


(* do we want a utils module with some of this stuff ? *)

let s = "c1a235aafbb6fa1e954a68b872d19611da0c7dc9" in
let () = Printf.printf "string %s\n" s in
let s1 = string_of_hex s in
Printf.printf "encoded %s\n" @@ btc_address_of_hash160 s1

let s = "30440220552e5d7a1ba2d3717f99ceea8a8b4f0caa99cd22a78cd66742a4bb25ede3e9bf02202bc116fb6d8169980b0a87b593f8b59b6430726f3ee85686ddf868c4a3c01a0001" in
let s = "030b7b19a00036b102336b53d473ab2c5e516bb5e7e668ceed799a711a3095fd97" in
let s = string_of_hex s in
let s = Message.sha256 s in 
let s = Message.ripemd160 s in 
let s = btc_address_of_hash160 s in
Printf.printf "encoded %s\n" s 



=======
let x = "c1a235aafbb6fa1e954a68b872d19611da0c7dc9" in 
let () = Printf.printf "original string %s\n" x in
let y = string_of_hex x in 
let y = "\x00" ^ y in
let y = y ^ ( Message.checksum2 y ) in
let () = Printf.printf "hex_of_string   %s\n" (Message.hex_of_string y) in
let z = Z.of_bits (Core.Core_string.rev y) in

let result = encode_base58 z  in

Printf.printf "z is %s\n"  result

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

>>>>>>> devel


