
(* ok, to verify the block, we have to be able to compute the hash, and 
	check it's deep enough.	
	and traces from genesis
*)


let decodeBlock (s:string) pos = 

	let pos, version = Message.decodeInteger32 s pos in 
	let pos, previous = Message.decodeHash32 s pos in
	let pos, merkle = Message.decodeHash32 s pos in
	let pos, nTime = Message.decodeInteger32 s pos in
	let pos, bits = Message.decodeInteger32 s pos in
	let pos, nonce = Message.decodeInteger32 s pos in

	let pos, tx_count = Message.decodeVarInt s pos in 
 
	let pos, tx = Message.decodeTx s pos in 
	let pos, tx2 = Message.decodeTx s pos in 

	

	let () = Printf.printf "hash     %s\n" (Message.strsub s 0 80 |> Message.sha256d  |> Message.strrev |> Message.hex_of_string ) in

	let () = Printf.printf "version  %x\n" version in
	let () = Printf.printf "previous %s\n" (Message.hex_of_string previous) in
	let () = Printf.printf "merkle   %s\n" (Message.hex_of_string merkle) in
	let () = Printf.printf "nTime    %d\n" ( nTime) in
	let () = Printf.printf "bits     %d\n" bits in
	let () = Printf.printf "nonce    %d\n" nonce in
	
	let () = Printf.printf "tx_count %d\n" tx_count in
	
	let () = Printf.printf "tx %s \n" ( Message.formatTx tx )in
	let () = Printf.printf "tx2 %s \n" ( Message.formatTx tx2 )in


	pos, ()


let block =
  let in_channel = open_in "blocks/000000000000000007c5b3e47c690e6ab9e75fdf1f47bfc7a247f29176be6d9f" in
  let s = Core.In_channel.input_all in_channel in
  let () = close_in in_channel in
  let _, block = decodeBlock s 0 in
  () 


