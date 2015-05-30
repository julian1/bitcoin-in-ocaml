
module M = Message


(* ok, so we just need to use this as private key... and compute public key from it
	we don't need to compute the 5-wif key, all we need are the compressed and uncompressed bitcoin addresses.
	so we could precompute ...

	- ok, strategy should be to load all btc addresses in memory.
	- and then start dict. because dict and variations will be larger...
	
 *)

let privkey = M.sha256 "a" in

let () = print_endline (M.hex_of_string privkey) in

let pubkey = match Microecc.compute_public_key privkey with 
	| None -> "none" 
	| Some s -> s
in
let () = print_endline (M.hex_of_string ("\x04" ^ pubkey)) in 

let compressed_pubkey = Microecc.compress pubkey in 
let () = print_endline (M.hex_of_string compressed_pubkey) in 



let addr_from_pubkey pubkey = 
	  pubkey 
	  |> Message.sha256 
	  |> Message.ripemd160 
	  |> Address.btc_address_of_hash160 in
	

let () = print_endline (addr_from_pubkey ("\x04" ^ pubkey)) in
let () = print_endline (addr_from_pubkey compressed_pubkey ) in

let () = print_endline "\n\n****" in

let rec range i j = if i > j then [] else i :: (range (i+1) j) in

(* about 2000 per second *)
let u = range 1 2000 in
let f i = 
	let s = string_of_int i in
	let privkey = M.sha256 s in
	match Microecc.compute_public_key privkey with 
		| None -> "none" 
		| Some pubkey -> addr_from_pubkey ("\x04" ^ pubkey) 
in

let addrs = List.map f u in 

let () = List.iter print_endline addrs in

(* needs something more for wif encoding. let () = print_endline ( Address.base58_of_string privkey ) in *) 
() 

