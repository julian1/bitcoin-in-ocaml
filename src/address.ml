
module M = Message

module CS = Core.Core_string

(* corebuild  -package cryptokit,zarith,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax address.byte *)

(* 
  what's next?
	- decode blocks, leveldb interface 
    - encode the public key... to show all tx inputs and outputs for pay to script. if bytes length is correct.
    - should change the format tx 

    - do script engine.
    - need secp256k1 in eec. or somewhere.  
*)

(* TODO should be factored out as generally useful function? *)
let z_of_string s = s |> CS.rev |> Z.of_bits

let string_of_z z = z |> Z.to_bits |> CS.rev


let code_string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" 

(* change name int value? *)
let code i = code_string.[i] 

let base58_of_string (value: string ) =
  (* TODO maybe replace list concat with Buffer for speed. 
  - we only need the Z type for arbitrary precision division/remainder
  might be able to implement ourselves in base 256. *)
  let code_string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
  let code i = code_string.[i] in
  let rec f acc value =
    if Z.gt value Z.zero then
      let div, rem = Z.div_rem value (Z.of_int 58) in
      f (code (Z.to_int rem)::acc) div 
    else
      acc
  in
  let rec zero_pad pos s acc =
    if s.[pos] == char_of_int 0 then
      zero_pad (pos+1) s (code 0::acc) 
    else
      acc
  in
  z_of_string value
    |> f []  
    |> zero_pad 0 value  (* TODO this doesn't look right *)
    |> Core.Std.String.of_char_list 



let code_value c = 
  let v = int_of_char c in
  match c with
    | '1' .. '9' -> v - int_of_char '1' 
    | 'A' .. 'H' -> v - int_of_char 'A' + 9  
    | 'J' .. 'N' -> v - int_of_char 'J' + 17  
    | 'P' .. 'Z' -> v - int_of_char 'P' + 22  
    | 'a' .. 'k' -> v - int_of_char 'a' + 33  
    | 'm' .. 'z' -> v - int_of_char 'm' + 44  

(* start at the back and add
    - multiply it out to get the z value.
    then string_of_z
    actually a fold...no because pos 
*)

let s = "1JeqjYhy7GzCMkbKZ7N9Um6usLNuhsjji1" 

(* rather than calculate rev index - should we just reverse the string first? *)
let len = String.length s in
let get i = String.get s (len - 1 -i) in
let rec f i z = 
  if i < len then 
    let c = get i in
    let value = code_value c in
    let () = Printf.printf "%c %d \n" c value in 
    let m = Z.pow (Z.of_int 58) i in
    f (i + 1) (Z.add z (Z.mul (Z.of_int value) m))
  else
    z 
in
let z = f 0 Z.zero in
let s = string_of_z z in
(* now we split out the 4 char checksum etc *)
print_endline (M.hex_of_string s )

(* we have to check zero padding *)


let rec f i = 
  if i < 58 then 
    let () = Printf.printf "%c  %i  %i\n" (code i) i  (code_value (code i)) in 
    f (i + 1) 
  else
    ()
in
f 0


let string_of_base58(value: string ) =

  (* is it a division, or do we just multi *)
  let rec f acc div =
    if Z.gt div Z.zero then
      let div, rem = Z.div_rem div (Z.of_int 58) in
      f (code (Z.to_int rem)::acc) div 
    else
      acc
  in
  let rec zero_pad pos s acc =
    if s.[pos] == char_of_int 0 then
      zero_pad (pos+1) s (code 0::acc) 
    else
      acc
  in
(* we can't do z of string at all, - instead to get the value we will have to add/multiply out the base58 value
  but we can check/compare with the input 

*)
  z_of_string value
    |> f []  
 (*   |> zero_pad 0 value *) 
    |> Core.Std.String.of_char_list 





let btc_address_of_hash160 (a: string) =
  let a = "\x00" ^ a in 
  let checksum = Message.checksum2 a in
  base58_of_string (a ^ checksum)


(* do we want a utils module with some of this stuff ? 
  ok, but we haven't got a one at the front of either of them...

  would seem to be a bug with the base58_of_string thing...
*)


(* this should be held in a test *)

let test1 () = 

	let s = M.string_of_hex "c1a235aafbb6fa1e954a68b872d19611da0c7dc9" in
	let () = Printf.printf "encoded %s\n" @@ btc_address_of_hash160 s in

	let s = "030b7b19a00036b102336b53d473ab2c5e516bb5e7e668ceed799a711a3095fd97" 
	  |> M.string_of_hex 
	  |> Message.sha256 
	  |> Message.ripemd160 
	  |> btc_address_of_hash160 in
	
  Printf.printf "encoded %s %d\n" s (Message.strlen s)


let ()  = test1 ()
