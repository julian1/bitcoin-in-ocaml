
open OUnit2;;

module M = Message



(* corebuild  -package microecc,cryptokit,zarith,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax test10.byte
  do we need to tell it the curve? 

	 corebuild -install-bin-dir   tests -no-links  -package microecc,cryptokit,zarith,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax tests/test10.byte

 *) 




let read_from_file filename =
  let in_channel = open_in filename in
  let s = Core.In_channel.input_all in_channel in
  let () = close_in in_channel in
  s


let tx_s = read_from_file "test_data/0e7b95f5640018b0255d840a7ec673d014c2cb2252641b629038244a6c703ecb" 
let _,tx = M.decodeTx tx_s 0 

(*
let () = Printf.printf "org %s\n" @@ M.hex_of_string tx_s
let () = Printf.printf "hash %s\n" ( tx_s |> M.sha256d |> M.strrev |> M.hex_of_string) 
*)



let txprev_s = read_from_file "test_data/d1ae76b9e9275fc88e3163dfba0a6bf5b3c8fe6a259a45a29e96a7c710777905" 
let _, txprev = M.decodeTx txprev_s 0 



(* let () = Printf.printf "%s\n" @@ M.formatTx tx *)

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
let () = Printf.printf "sig %s\n" @@ M.hex_of_string signature
let () = Printf.printf "key %s\n" @@ M.hex_of_string pubkey 
let () = Printf.printf "subscript %s\n" @@ M.format_script subscript
*)


(* set input scripts to empty *)
let tx_clear_inputs (tx: M.tx ) = 
	let clear_input_script (input : M.tx_in) = 
	{ input  with
		script = M.encode_script []	
	} in
	{ tx with inputs = List.map clear_input_script tx.inputs }


(* should be able to use mapi if we have it *)

let tx = tx_clear_inputs tx


(* substitute input script, and clear others *)
let tx = 
	let f index (input: M.tx_in ) = 
		if index == 0 then { input with script = M.encode_script subscript; }  
		else { input with script = M.encode_script [] } 
	in
	{ tx with inputs = List.mapi f tx.inputs }  


(*
let () = Printf.printf "%s\n" @@ M.formatTx (tx )
*)

(* ok, now we need a re-encode tx function and we need to append 1 byte to end before hasing *)
let s = M.encodeTx tx
let s = s ^ M.encodeInteger32 1 (* "\x01" *)  (* should be single byte not string - adding hash type *)

(* let hash = ( s |> M.sha256d |> M.strrev |> M.hex_of_string)  *)
let hash = ( s |> M.sha256d (*|> M.strrev *) ) 


(* Ok, i think we actually need to use real values to debug this behavior *)


let test1 test_ctxt = 
	let r,s = M.decode_der_signature signature in
    let decoded_sig = r ^ s in
	let x = Microecc.verify pubkey hash decoded_sig in

	let () = Printf.printf "sig result %b\n" x in
	assert_equal x true 
	

let tests =
 ["test10">::: 
	 ["test1">:: test1; 
		"test2">:: test1; ]
	]
;;

let r,s = M.decode_der_signature signature in
let decoded_sig = r ^ s in
let x = Microecc.verify pubkey hash decoded_sig in
let () = Printf.printf "sig result %b\n" x in
()







