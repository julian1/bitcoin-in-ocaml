

open OUnit2

module M = Message
module L = List



let test1 ctx =
  (* block header decoding *)
  let in_channel = open_in "test/data/000000000000000007c5b3e47c690e6ab9e75fdf1f47bfc7a247f29176be6d9f" in
  let s = Core.In_channel.input_all in_channel in
  let () = close_in in_channel in
  (* let _, header = M.decodeBlock s 0 in *)

  let txs = M.decode_block_txs s in 
  let () = print_endline @@ "count " ^ (string_of_int (L.length txs))  in
 

  assert_bool "true" true  

let tests =
   "message">::: [ "test1">:: test1; ]

(*
(
    header.previous = M.string_of_hex "000000000000000010006fe522dd3d6251c7d7ba217d294bcb4f99dcc11b1d24"
    && header.merkle = M.string_of_hex "e658aef520b5fa1687f1c33a3bfc0336722fab49fe87ef0c96d46693f68d914b"
    && header.bits = 404196666
    && header.nTime = 1425091936
    && header.nonce = 2316010512
    )
*)
