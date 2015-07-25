

open OUnit2

module M = Message
module L = List
module A = Address

let (<|) f g x = f(g(x))


let read_file filename =
  let in_channel = open_in filename in
  let s = Core.In_channel.input_all in_channel in
  let () = close_in in_channel in
  s


let compare a b =
  let za = A.z_of_string a in
  let zb = A.z_of_string b in
  if Z.gt za zb then 1
  else if Z.equal za zb then 0
  else -1


(* fold isn't right - unless we do a modulo or something - it has to skip over the list two at a time 

  we should be able to gobble it up recursively with patterns

  ok, we can fold
*)
let rec f lst =
  match lst with 
    | e :: [] -> e
    | first::tail ->

      let aggregate a b =
        a ^ b |> M.sha256d |> M.strrev 
      in
      let lst = L.fold_left aggregate first tail in
      lst

let test1 ctx =
  M.(
  let s = read_file "test/data/000000000000000007c5b3e47c690e6ab9e75fdf1f47bfc7a247f29176be6d9f" in 
  (* let _, header = M.decodeBlock s 0 in *)
  let txs = M.decode_block_txs s in 
  let () = print_endline @@ "count " ^ (string_of_int <| L.length) txs in
  let hash_of_tx tx = S.sub s tx.pos tx.length |> M.sha256d |> M.strrev in 
  let txs = L.map hash_of_tx txs in
  let txs = L.sort compare txs in  

  (* let () = print_endline @@ M.hex_of_string ( L.hd txs) in *)
  let _ = L.fold_left (fun acc hash -> print_endline @@ M.hex_of_string hash) () txs in
 
  assert_bool "true" true  
  )


let tests =
   "message">::: [ "test1">:: test1; ]

(*

(* fold isn't right - unless we do a modulo or something - it has to skip over the list two at a time *)
let rec f lst =
  match lst with 
    | e :: [] -> e
    | first::tail ->

      let aggregate a b =
        a ^ b |> M.sha256d |> M.strrev 
      in
      let lst = L.fold_left aggregate first tail in
      lst


  ok, now we want to sort the list...

(
    header.previous = M.string_of_hex "000000000000000010006fe522dd3d6251c7d7ba217d294bcb4f99dcc11b1d24"
    && header.merkle = M.string_of_hex "e658aef520b5fa1687f1c33a3bfc0336722fab49fe87ef0c96d46693f68d914b"
    && header.bits = 404196666
    && header.nTime = 1425091936
    && header.nonce = 2316010512
    )
*)
