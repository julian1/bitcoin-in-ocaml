
(* corebuild  -package microecc,cryptokit,zarith,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax test10.byte
  do we need to tell it the curve? 
 *) 

(*
30 44 
  02 20 552e5d7a1ba2d3717f99ceea8a8b4f0caa99cd22a78cd66742a4bb25ede3e9bfj
  02 20 2bc116fb6d8169980b0a87b593f8b59b6430726f3ee85686ddf868c4a3c01a0001
*)

(* let signature = Address.string_of_hex "30440220552e5d7a1ba2d3717f99ceea8a8b4f0caa99cd22a78cd66742a4bb25ede3e9bf02202bc116fb6d8169980b0a87b593f8b59b6430726f3ee85686ddf868c4a3c01a0001"
*)
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

	
(* so we need to decode the real transaction 
	and then recode it...
*)

(*	
let signature = decode_der_signature signature

(* is this der encoded ?? *)
let public_key1 = Address.string_of_hex "030b7b19a00036b102336b53d473ab2c5e516bb5e7e668ceed799a711a3095fd97"  
let () = Printf.printf "public_key length %d\n" (Message.strlen public_key1 )

let public_key = Microecc.decompress public_key1 
let () = Printf.printf "decompressed public_key length %d\n" (Message.strlen public_key )

(* let () = print_endline ("decompressed " ^ Message.hex_of_string public_key ) *)

let hash = (String.make 31 '\x00') ^ "\x01";;

let result = Microecc.verify public_key hash signature  

let _ = match result with
  | true -> print_endline "PASSED"; true
  | false -> print_endline "FAILED: Signature verification failed"; false
*)

let tx  =
  let in_channel = open_in "test_data/0e7b95f5640018b0255d840a7ec673d014c2cb2252641b629038244a6c703ecb" in
  let s = Core.In_channel.input_all in_channel in
  let () = close_in in_channel in
  let _, tx = Message.decodeTx s 0 in
  tx

let () = Printf.printf "%s\n" @@ Message.formatTx tx

let signature = match tx.inputs with { script } ::_ -> match List.hd script with Bytes s -> s
let pubkey    = match tx.inputs with { script } ::_ -> match List.nth script 1 with Bytes s -> s

let () = Printf.printf "sig %s\n" @@ Message.hex_of_string signature
let () = Printf.printf "key %s\n" @@ Message.hex_of_string pubkey 

(* set input scripts to empty *)
let tx_copy (tx: Message.tx )  = 
	let clear_input_script (input : Message.tx_in)= 
		{ input  with
			script = []	
		} in
	{ tx with inputs = List.map clear_input_script tx.inputs }



let () = Printf.printf "%s\n" @@ Message.formatTx (tx_copy tx )

(*
	{ tx with 
	(* inputs is a list so this won't work, think we want to map tx.inputs *)
	inputs = { tx.inputs with
	  previous = : string ;
	  index : int ; 
	  script: script_token list ; 
	  sequence : int ; 
	}
} 	
*)


