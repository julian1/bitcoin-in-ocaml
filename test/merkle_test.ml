

open OUnit2

module M = Message
module L = List
module A = Address

let (<|) f g x = f(g(x))



(*
let compare a b =
  let za = A.z_of_string a in
  let zb = A.z_of_string b in
  if Z.gt za zb then 1
  else if Z.equal za zb then 0
  else -1
*)

let hash tx = tx |> M.sha256d |> M.strrev


let concat_hash a b = hash (M.strrev a ^ M.strrev b)


let rec merkle lst =
  match lst with
    | e :: [] -> e
    | lst ->
      let lst =
        if (L.length lst) mod 2 = 1 then
          (* duplicate last element *)
          let lst  = L.rev lst in
          L.hd lst :: lst |> L.rev
        else
          lst
      in
      let aggregate (lst,previous) e =
        match previous with
          | None -> lst, Some e
          | Some e2 -> concat_hash e2 e::lst, None
      in
      let lst, None = L.fold_left aggregate ([],None) lst in
      let lst = L.rev lst in
      merkle lst



let test1 ctx =
  let a = M.string_of_hex "0000000000000000000000000000000000000000000000000000000000000000" in
  let b = M.string_of_hex "0000000000000000000000000000000000000000000000000000000000000011" in
  let ret = concat_hash a b in
  let expected = M.string_of_hex "32650049a0418e4380db0af81788635d8b65424d397170b8499cdc28c4d27006" in
  assert_equal ret expected


let test2 ctx =
  let lst = [
    M.string_of_hex "0000000000000000000000000000000000000000000000000000000000000000";
    M.string_of_hex "0000000000000000000000000000000000000000000000000000000000000011";
    M.string_of_hex "0000000000000000000000000000000000000000000000000000000000000022";
  ] in
  let ret = merkle lst in
  let expected = M.string_of_hex "d47780c084bad3830bcdaf6eace035e4c6cbf646d103795d22104fb105014ba3" in
  assert_equal ret expected


let read_file filename =
  let in_channel = open_in filename in
  let s = Core.In_channel.input_all in_channel in
  let () = close_in in_channel in
  s


let test3 ctx =
  M.(
    let s = read_file "test/data/000000000000000007c5b3e47c690e6ab9e75fdf1f47bfc7a247f29176be6d9f" in
    let _, header = M.decodeBlock s 0 in
    let txs = M.decode_block_txs s in
    let hash_of_tx tx = S.sub s tx.pos tx.length |> hash in
    let txs = L.map hash_of_tx txs in
    (* let txs = L.sort compare txs in *)
    let ret = merkle txs in
    assert_equal header.merkle ret
  )

let tests =
   "message">::: [ "test1">:: test1; "test2">:: test2;  "test3">:: test3; ]



(*
  let () = print_endline @@ "count " ^ (string_of_int <| L.length) txs in
  let () = print_endline @@ "merkle " ^ M.hex_of_string header.merkle in
    let () = print_endline @@ "ret " ^ M.hex_of_string ret in

  let _ = L.fold_left (fun _ hash -> print_endline @@ M.hex_of_string hash) () txs in

*)
