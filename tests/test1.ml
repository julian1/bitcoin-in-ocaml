
(* decode a block *)

open OUnit2

let decodeJ (s:string) pos = 
	let pos, block = Message.decodeBlock s pos in 
	let pos, tx = Message.decodeTx s pos in 
	let pos, tx2 = Message.decodeTx s pos in 
	let () = Printf.printf "%s\n" ( Message.formatBlock block ) in 
(*	let () = Printf.printf "tx %s \n" ( Message.formatTx tx )in
	let () = Printf.printf "tx2 %s \n" ( Message.formatTx tx2 )in *)
	pos, block 

let test1 test_ctxt =
  let in_channel = open_in "test_data/000000000000000007c5b3e47c690e6ab9e75fdf1f47bfc7a247f29176be6d9f" in
  let s = Core.In_channel.input_all in_channel in
  let () = close_in in_channel in
  let _, block_ = decodeJ s 0 in
  assert_equal 1425091936 (block_.nTime )

let tests = ["test1">:: test1; ]


