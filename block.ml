
let decodeBlock (s:string) pos = 

	let pos, version = Message.decodeInteger32 s pos in 
	let pos, previous = Message.decodeHash32 s pos in
	let pos, merkle = Message.decodeHash32 s pos in
	let pos, nTime = Message.decodeInteger32 s pos in
	let pos, bits = Message.decodeInteger32 s pos in
	let pos, nonce = Message.decodeInteger32 s pos in

	let pos, tx_count = Message.decodeVarInt s pos in 
 
 
	let () = Printf.printf "version  %x\n" version in
	let () = Printf.printf "previous %s\n" (Message.hex_of_string previous) in
	let () = Printf.printf "merkle   %s\n" (Message.hex_of_string merkle) in
	let () = Printf.printf "nTime    %d\n" ( nTime) in
	let () = Printf.printf "bits     %d\n" bits in
	let () = Printf.printf "nonce    %d\n" nonce in
	
	let () = Printf.printf "tx_count %d\n" tx_count in

	pos, ()


let block =
  let in_channel = open_in "blocks/8771ea1a82adb1b775ee80d50d44aa2a21b1056bd9ddc0553f4c629c1ca78f77" in
  let s = Core.In_channel.input_all in_channel in
  let () = close_in in_channel in
  let _, block = decodeBlock s 0 in
  () 


