(*
	corebuild -I src -package leveldb,microecc,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax  harvester/test.byte

	So. create a leveldb of all the hash160
*)


module M = Message


(* ok, so we just need to use this as private key... and compute public key from it
	we don't need to compute the 5-wif key, all we need are the compressed and uncompressed bitcoin addresses.
	so we could precompute ...

	- ok, strategy should be to load all btc addresses in memory.
	- and then start dict. because dict and variations will be larger...
------

	- all right we have to get bitcoin addresses indexed, in a way we can use easily...
	- either leveldb, sql, or flat file etc.

	- address -> ( transaction amount ) 
	- remember the primary 

	- if it's a flat file, then we can record sequentially.

	- deposit.
	address, tx ammount
	address, tx ammount 

	- should we index the other way around?	 by tx and then just reverse on output? 

	- we have a structure that's (tx, index) to record if it's spent or unspent...
		and just remove...

	(tx, index) -> (amount, address(es) ) 

	-----
	- no it's a parallel structure 
		tx, index for outputs. 
		address -> ammount    (+-)

	- we just increment and decrement the value on the address 
*)

(* let privkey = M.sha256 "Barbadoss" |> M.sha256 in *)
let privkey = M.sha256 "bob" in

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

