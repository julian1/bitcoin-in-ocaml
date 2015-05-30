
module M = Message


(* ok, so we just need to use this as private key... and compute public key from it
	
	we don't need to compute the 5-wif key, all we need are the compressed and uncompressed bitcoin addresses.
 *)

let privkey = M.sha256 "" in

let () = print_endline (M.hex_of_string privkey) in

let pubkey = match Microecc.compute_public_key privkey with 
	| None -> "none" 
	| Some s -> "\x04" ^ s
in
(* 
	der encoded - 04a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235 
	ours            a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235

*)
let () = print_endline (M.hex_of_string pubkey) in 

let addr = 
	  pubkey 
	  |> Message.sha256 
	  |> Message.ripemd160 
	  |> Address.btc_address_of_hash160 in
	

print_endline (addr ) 

