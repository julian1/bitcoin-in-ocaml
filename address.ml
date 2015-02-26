
(* corebuild  -package zarith,sha,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax address.byte *)

(* 
  what's next?
    - encode the public key... to show all tx inputs and outputs for pay to script. if bytes length is correct.
    - then do script engine.
*)


let base58_of_string (value: string ) =
  (* TODO maybe replace list concat with Buffer for speed. 
  - we only need the Z type for arbitrary precision division/remainder
  might be able to implement ourselves in base 256. *)
  let code_string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
  let rec f div acc =
    if Z.gt div Z.zero then
      let div1, rem = Z.div_rem div (Z.of_int 58) in
      f div1 (code_string.[Z.to_int rem]::acc)
    else
      acc
  in
  let rec zero_pad pos s acc =
    if s.[pos] == char_of_int 0 then
      zero_pad (pos+1) s (code_string.[0]::acc) 
    else
      acc
  in
  Z.of_bits (Core.Core_string.rev value) 
    |> (fun z -> f z []) 
    |> zero_pad 0 value 
    |> Core.Std.String.of_char_list 


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
  let n = String.length s in
  let buf = Buffer.create (n/2) in
  for i = 0 to n/2-1 do
    let i2 = i * 2 in
    let x = int_of_hex s.[i2] in
    let y = int_of_hex s.[i2+1] in
    Buffer.add_char buf @@ char_of_int (x lsl 4 + y)
  done;
  Buffer.contents buf


let btc_address_of_hash160 (a: string) =
  let a = "\x00" ^ a in 
  let checksum = Message.checksum2 a in
  base58_of_string (a ^ checksum)


(* do we want a utils module with some of this stuff ? 
  ok, but we haven't got a one at the front of either of them...

  would seem to be a bug with the base58_of_string thing...
*)

let s = "c1a235aafbb6fa1e954a68b872d19611da0c7dc9" in
let s = string_of_hex s in
Printf.printf "encoded %s\n" @@ btc_address_of_hash160 s

let s = "030b7b19a00036b102336b53d473ab2c5e516bb5e7e668ceed799a711a3095fd97" in
let s = string_of_hex s in
let s = Message.sha256 s in 
let s = Message.ripemd160 s in 
let s = btc_address_of_hash160 s in
Printf.printf "encoded %s %d\n" s (Message.strlen s)





