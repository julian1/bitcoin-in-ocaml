(* 
  corebuild -I src  -package pgocaml,microecc,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax  src/client3.byte
 *) 


module M = Message

open M




let read_from_file filename =
  let in_channel = open_in filename in
  let s = Core.In_channel.input_all in_channel in
  let () = close_in in_channel in
  s

let tx_s = read_from_file "test_data/0e7b95f5640018b0255d840a7ec673d014c2cb2252641b629038244a6c703ecb" 
let txprev_s = read_from_file "test_data/d1ae76b9e9275fc88e3163dfba0a6bf5b3c8fe6a259a45a29e96a7c710777905" 


(* substitute input script, and clear others *)
let substitute tx i output_script = 
	let f index (input: M.tx_in ) = 
		if index == i then { input with script = M.encode_script output_script; }  
		else { input with script = M.encode_script [] } 
	in
	{ tx with inputs = List.mapi f tx.inputs }  


let encode_and_hash tx =
  (* ok, now we need a re-encode tx function and we need to append 1 byte to end before hasing *)
  let s = M.encodeTx tx ^ M.encodeInteger32 1 in
  let hash = M.sha256d s in
  hash

(* let () = Printf.printf "%s\n" @@ M.formatTx tx *)

(* must be an easier way to drill down into the scripts that we want *)

let _, txprev = M.decodeTx txprev_s 0 
let output = List.nth txprev.outputs 0
let output_script = M.decode_script output.script 

let _,tx = M.decodeTx tx_s 0 
let input = List.nth tx.inputs 0 
let input_script = M.decode_script input.script 

let signature,pubkey = match input_script with 
  | M.BYTES s :: M.BYTES p :: [] -> s, p

(* how do we know whether to decompress the pubkey? *)
let pubkey = Microecc.decompress pubkey 

(* so we substitute the prev output script into current tx with its outputs *)   
let tx = substitute tx 0 output_script in 
let hash = encode_and_hash tx in
	
(*
let () = Printf.printf "%s\n" @@ M.formatTx (tx )
*)


let Some (r,s) = M.decode_der_signature signature in
let decoded_sig = r ^ s in
let x = Microecc.verify pubkey hash decoded_sig in
let () = Printf.printf "sig result %b\n" x in
()







