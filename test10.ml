
(* corebuild  -package microecc,cryptokit,zarith,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax test10.byte
  do we need to tell it the curve? 
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





let read_from_file filename =
  let in_channel = open_in filename in
  let s = Core.In_channel.input_all in_channel in
  let () = close_in in_channel in
  s


let tx_s = read_from_file "test_data/0e7b95f5640018b0255d840a7ec673d014c2cb2252641b629038244a6c703ecb" 
let _,tx = Message.decodeTx tx_s 0 

let () = Printf.printf "org %s\n" @@ Message.hex_of_string tx_s

let s = Message.encodeTx tx

let () = Printf.printf "enc %s\n" @@ Message.hex_of_string s

let () = exit 0



let txprev_s = read_from_file "test_data/d1ae76b9e9275fc88e3163dfba0a6bf5b3c8fe6a259a45a29e96a7c710777905" 
let _, txprev = Message.decodeTx txprev_s 0 



(* let () = Printf.printf "%s\n" @@ Message.formatTx tx *)

(* must be an easier way to drill down into the scripts that we want *)
let subscript = (List.hd txprev.outputs).script 

let tx_input_script = (List.hd tx.inputs).script 
let signature = match List.hd tx_input_script with Bytes s -> s 
let pubkey    = match List.nth tx_input_script 1 with Bytes s -> s

let () = Printf.printf "sig %s\n" @@ Message.hex_of_string signature
let () = Printf.printf "key %s\n" @@ Message.hex_of_string pubkey 
let () = Printf.printf "subscript %s\n" @@ Message.format_script subscript


(* set input scripts to empty *)
let tx_clear_inputs (tx: Message.tx ) = 
	let clear_input_script (input : Message.tx_in) = 
	{ input  with
		script = []	
	} in
	{ tx with inputs = List.map clear_input_script tx.inputs }


(* should be able to use mapi if we have it *)

let tx = tx_clear_inputs tx


(* substitute input script, and clear others *)
let tx = 
	let f index (input: Message.tx_in ) = 
		if index == 0 then { input with script = subscript; }  
		else { input with script = [] } 
	in
	{ tx with inputs = List.mapi f tx.inputs }  


let () = Printf.printf "%s\n" @@ Message.formatTx (tx )

(* ok, now we need a re-encode tx function and we need to append 1 byte to end before hasing *)


let decoded_sig = decode_der_signature signature

