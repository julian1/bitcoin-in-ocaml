
(* corebuild  -package microecc,cryptokit,zarith,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax test10.byte
  
  do we need to tell it the curve? 
 *) 

(*
30 44 
  02 20 552e5d7a1ba2d3717f99ceea8a8b4f0caa99cd22a78cd66742a4bb25ede3e9bfj
  02 20 2bc116fb6d8169980b0a87b593f8b59b6430726f3ee85686ddf868c4a3c01a0001
*)

let signature = Address.string_of_hex "30440220552e5d7a1ba2d3717f99ceea8a8b4f0caa99cd22a78cd66742a4bb25ede3e9bf02202bc116fb6d8169980b0a87b593f8b59b6430726f3ee85686ddf868c4a3c01a0001"

let decode_der_signature s =
	let () = Printf.printf "real length %d \n" (Message.strlen s) in
	let pos = 0 in
	let pos, structure = Message.decodeInteger8 s pos in 
	let pos, length = Message.decodeInteger8 s pos in 
	let () = Printf.printf "length %d\n" length  in
	let pos, _0x02 = Message.decodeInteger8 s pos in 
	let () = Printf.printf "%d\n" _0x02 in
	let pos, r_length = Message.decodeInteger8 s pos in 
	let () = Printf.printf "r_length %d\n" r_length in
	let pos, r = Message.decs_ s pos r_length in 
	let () = Printf.printf "r %s\n" (Message.hex_of_string r) in
	let pos, _0x02 = Message.decodeInteger8 s pos in 
	let () = Printf.printf "%d\n" _0x02 in
	let pos, s_length = Message.decodeInteger8 s pos in 
	let () = Printf.printf "s_length %d\n" s_length in
	let pos, s_ = Message.decs_ s pos s_length in 
	let () = Printf.printf "s_ %s\n" (Message.hex_of_string s_) in
	let pos, sigType = Message.decodeInteger8 s pos in 
	let () = Printf.printf "sigType %d\n" sigType in 
	r ^ s_

		
let signature = decode_der_signature signature

(* is this der encoded ?? *)
let public_key1 = Address.string_of_hex "030b7b19a00036b102336b53d473ab2c5e516bb5e7e668ceed799a711a3095fd97"  
let () = Printf.printf "public_key length %d\n" (Message.strlen public_key1 )


let public_key = Microecc.decompress public_key1 
let () = Printf.printf "decompressed public_key length %d\n" (Message.strlen public_key )

(* let () = print_endline ("decompressed " ^ Message.hex_of_string public_key ) *)

(*
(* perhaps the hash is the tx *)
let hash = Address.string_of_hex "d1ae76b9e9275fc88e3163dfba0a6bf5b3c8fe6a259a45a29e96a7c710777905" (* "0e7b95f5640018b0255d840a7ec673d014c2cb2252641b629038244a6c703ecb" *)
*)

let hash = (String.make 31 '\x00') ^ "\x01";;

let result = Microecc.verify public_key hash signature  


match result with
  | true -> print_endline "PASSED"; true
  | false -> print_endline "FAILED: Signature verification failed"; false
;
