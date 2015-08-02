
module M = Message
module L = List
module U = Util 




let (>>=) = Lwt.(>>=) 
let return = Lwt.return

let (<|) f g x = f(g(x))



let log s = U.write_stdout s

type block_ =
{
    payload : string;
    hash : string;      (* avoid recalculation when store_block *)
    height : int;
    difficulty : int;  (* should be a z value? stored as string?  *)    
}

(* 
    we can log...  to stdout ...

    - merkle.
    - check hash is less than difficulty(don't know how this works.... with litecoin and doge )
    - check difficulty is correct for height and network parameters  
  -----
  
    we should get the height stuff in because that's expensive to calculate leaves ...
*)

let calc_merkle txs payload =
  let hash_ = M.strrev <| M.sha256d in 
  let hash_tx tx = M.(S.sub payload tx.pos tx.length) |> hash_ in
  let txs = L.map hash_tx txs in
  Merkle.root txs 


let validate_block db payload = 
  let hash = M.decode_block_hash payload in
  let _, header = M.decodeBlock payload 0 in
  let txs = M.decode_block_txs payload in
 
  let merkle_ok = header.merkle = calc_merkle txs payload in
  log @@ "merkle " ^ string_of_bool merkle_ok 
  >>
  let difficulty_ok = true in

  match merkle_ok && difficulty_ok with 
    | true -> 
      return (Some { 
        payload = payload; 
        hash = hash;
        height = 0;
        difficulty = 0;
      })
    | false ->
      return None
 
  
