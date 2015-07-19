
module M = Message


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
let z_of_string s = s |> Core.Core_string.rev |> Z.of_bits


let base58_of_string (value: string ) =
  (* TODO maybe replace list concat with Buffer for speed. 
  - we only need the Z type for arbitrary precision division/remainder
  might be able to implement ourselves in base 256. *)
  let code_string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
  let rec f acc div =
    if Z.gt div Z.zero then
      let div1, rem = Z.div_rem div (Z.of_int 58) in
      f (code_string.[Z.to_int rem]::acc) div1 
    else
      acc
  in
  let rec zero_pad pos s acc =
    if s.[pos] == char_of_int 0 then
      zero_pad (pos+1) s (code_string.[0]::acc) 
    else
      acc
  in
  z_of_string value  
    |> f []  
    |> zero_pad 0 value 
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

	let s = "c1a235aafbb6fa1e954a68b872d19611da0c7dc9" in
	let s = M.string_of_hex s in
	let () = Printf.printf "encoded %s\n" @@ btc_address_of_hash160 s in

	let s = "030b7b19a00036b102336b53d473ab2c5e516bb5e7e668ceed799a711a3095fd97" 
	  |> M.string_of_hex 
	  |> Message.sha256 
	  |> Message.ripemd160 
	  |> btc_address_of_hash160 in
	
  Printf.printf "encoded %s %d\n" s (Message.strlen s)


