
open OUnit2;;

module M = Message



(* corebuild  -package microecc,cryptokit,zarith,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax test10.byte
  do we need to tell it the curve? 

	 corebuild -install-bin-dir   tests -no-links  -package microecc,cryptokit,zarith,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax tests/test10.byte

real length 71
length 68
02 2
r_length 32
r 552e5d7a1ba2d3717f99ceea8a8b4f0caa99cd22a78cd66742a4bb25ede3e9bf
2
s_length 32
s_ 2bc116fb6d8169980b0a87b593f8b59b6430726f3ee85686ddf868c4a3c01a00
sigType 1
sig result true

 *) 

let decode_der_signature s =

    let decode_elt s pos = 
        let pos, _0x02 = Message.decodeInteger8 s pos in 
        let () = Printf.printf "02 %d\n" _0x02 in
        let pos, r_length = Message.decodeInteger8 s pos in 
        let () = Printf.printf "r_length %d\n" r_length in
        let pos, r = Message.decs_ s pos r_length  in
        (pos, r)
    in

	let () = Printf.printf "real length %d \n" (Message.strlen s) in
	let pos = 0 in
	let pos, structure = Message.decodeInteger8 s pos in 
    let pos, length = Message.decodeInteger8 s pos in 
    let () = Printf.printf "length %d\n" length  in


    let pos, r = decode_elt s pos in  
	let () = Printf.printf "r %s\n" (Message.hex_of_string r) in

    let pos, s_ = decode_elt s pos in  
	let () = Printf.printf "s %s\n" (Message.hex_of_string s_) in


	let pos, sigType = Message.decodeInteger8 s pos in 
	let () = Printf.printf "sigType %d\n" sigType in 
	(* (Message.strrev r) ^ (Message.strrev s_ ) *)
	r ^  s_ 





let read_from_file filename =
  let in_channel = open_in filename in
  let s = Core.In_channel.input_all in_channel in
  let () = close_in in_channel in
  s


let tx_s = read_from_file "test_data/0e7b95f5640018b0255d840a7ec673d014c2cb2252641b629038244a6c703ecb" 
let _,tx = Message.decodeTx tx_s 0 

(*
let () = Printf.printf "org %s\n" @@ Message.hex_of_string tx_s
let () = Printf.printf "hash %s\n" ( tx_s |> Message.sha256d |> Message.strrev |> Message.hex_of_string) 
*)



let txprev_s = read_from_file "test_data/d1ae76b9e9275fc88e3163dfba0a6bf5b3c8fe6a259a45a29e96a7c710777905" 
let _, txprev = Message.decodeTx txprev_s 0 



(* let () = Printf.printf "%s\n" @@ Message.formatTx tx *)

(* must be an easier way to drill down into the scripts that we want *)
let subscript = M.decode_script @@ (List.hd txprev.outputs).script 

let tx_input_script = M.decode_script @@ (List.hd tx.inputs).script 

(* do these need to be reversed ?? 
	eg. reverse sig and then 
	Not according to blockchain info
*)

let signature = match List.hd tx_input_script with BYTES s -> s 
let pubkey    = match List.nth tx_input_script 1 with BYTES s -> s

let pubkey = Microecc.decompress pubkey 


(*
let () = Printf.printf "sig %s\n" @@ Message.hex_of_string signature
let () = Printf.printf "key %s\n" @@ Message.hex_of_string pubkey 
let () = Printf.printf "subscript %s\n" @@ Message.format_script subscript
*)


(* set input scripts to empty *)
let tx_clear_inputs (tx: Message.tx ) = 
	let clear_input_script (input : Message.tx_in) = 
	{ input  with
		script = M.encode_script []	
	} in
	{ tx with inputs = List.map clear_input_script tx.inputs }


(* should be able to use mapi if we have it *)

let tx = tx_clear_inputs tx


(* substitute input script, and clear others *)
let tx = 
	let f index (input: Message.tx_in ) = 
		if index == 0 then { input with script = M.encode_script subscript; }  
		else { input with script = M.encode_script [] } 
	in
	{ tx with inputs = List.mapi f tx.inputs }  


(*
let () = Printf.printf "%s\n" @@ Message.formatTx (tx )
*)

(* ok, now we need a re-encode tx function and we need to append 1 byte to end before hasing *)
let s = Message.encodeTx tx
let s = s ^ Message.encodeInteger32 1 (* "\x01" *)  (* should be single byte not string - adding hash type *)

(* let hash = ( s |> Message.sha256d |> Message.strrev |> Message.hex_of_string)  *)
let hash = ( s |> Message.sha256d (*|> Message.strrev *) ) 


(* Ok, i think we actually need to use real values to debug this behavior *)


let test1 test_ctxt = 
	let decoded_sig = decode_der_signature signature in
	let x = Microecc.verify pubkey hash decoded_sig in

	let () = Printf.printf "sig result %b\n" x in
	assert_equal x true 
	

let tests =
 ["test10">::: 
	 ["test1">:: test1; 
		"test2">:: test1; ]
	]
;;

	let decoded_sig = decode_der_signature signature in
	let x = Microecc.verify pubkey hash decoded_sig in

	let () = Printf.printf "sig result %b\n" x in
    ()
	






