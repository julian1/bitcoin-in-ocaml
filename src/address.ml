
module M = Message

module S = String
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
let z_of_string s = 
  s |> CS.rev 
    |> Z.of_bits


(* take care to truncate leading 0's due to z internal representation *)
let string_of_z z = 
  z |> Z.to_bits 
    |> CS.rev 
    |> CS.lstrip ~drop:((=)(char_of_int 0)) 


let code58_of_int i = 
  S.get "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" i 
    
 
let count_prefix_chars s ch i =
  let rec f i =
    if S.get s i = ch then
      f (succ i)
    else
      i 
  in f i


let base58_of_string (s : string) =
  let rec f acc z =
    if Z.gt z Z.zero then
      let div, rem = Z.div_rem z (Z.of_int 58) in
      f (code58_of_int (Z.to_int rem)::acc) div 
    else
      acc
  in
  let ret = s 
    |> z_of_string 
    |> f [] 
    |> CS.of_char_list  in 
  let n = count_prefix_chars s (char_of_int 0) 0 in
  S.init n (fun _ -> code58_of_int 0) ^ ret 


let int_of_code58 c = 
  let v = int_of_char c in
  match c with
    | '1' .. '9' -> v - int_of_char '1' 
    | 'A' .. 'H' -> v - int_of_char 'A' + 9  
    | 'J' .. 'N' -> v - int_of_char 'J' + 17  
    | 'P' .. 'Z' -> v - int_of_char 'P' + 22  
    | 'a' .. 'k' -> v - int_of_char 'a' + 33  
    | 'm' .. 'z' -> v - int_of_char 'm' + 44  


let string_of_base58 (s : string) =
  let len = S.length s in
  let get i = S.get s (len - 1 -i) in 
  let rec f i z = 
    if i < len then 
      let c = get i in
      let value = int_of_code58 c in
      let b = Z.pow (Z.of_int 58) i in
      f (succ i) (Z.add z (Z.mul (Z.of_int value) b))
    else
      z 
  in
  let ret = f 0 Z.zero 
    |> string_of_z in
  let n = count_prefix_chars s (code58_of_int 0) 0 in
  S.init n (fun _ -> char_of_int 0) ^ ret

  
 

let s = string_of_base58 "1JeqjYhy7GzCMkbKZ7N9Um6usLNuhsjji1" in 
let () = print_endline (M.hex_of_string s) in

let s = base58_of_string s in
let () = print_endline s in
()




let btc_address_of_hash160 (a: string) =
  let a = "\x00" ^ a in 
  let checksum = M.checksum2 a in
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

(*
let rec f i = 
  if i < 58 then 
    let () = Printf.printf "%c  %i  %i\n" (code58_of_int i) i  (int_of_code58 (code58_of_int i)) in 
    f (i + 1) 
  else
    ()
in
f 0
*)
(* 
    - there's an issue, that we'll end up with a bunch of prefix zeros
    due to the string_of_z which is the internal byte representation ... 
    - think we may want to strip this...
*)


(* let s = "111121JeqjYhy7GzCMkbKZ7N9Um6usLNuhsjji1"  *)

(* rather than calculate rev index - should we just reverse the string first? or count down? 

  is there a way to could the leading zero's somehow ? rather than patch it up later?  

  check things like,
  \x0000000000000000000000002a072728b6665500 = 1111111111111o111111111cLV3wA 
*)

(*
  functions at the start and then calculation at the end
*)


