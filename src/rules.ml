
(* change filename to protocol.ml ? *)

module M = Message
module L = List
module U = Util 
module S = String



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


(*
    should look at the first tx and see if it's correct.
    auxpow - is bitchy.
    the aux parent block hash is in the aux tx ...  
    - lets just try to get the rest of the transactions...

    - ok, we stuffed something up. and it's likely we stuffed wrongly decoded tx's in the database.  
*)




let validate_block network db payload = 
    let hash = M.decode_block_hash payload in
    log @@ "hash " ^ M.hex_of_string hash
  >>
    let pos, header = M.decodeBlock network payload 0 in
    log @@ "payload length " ^ (string_of_int <| S.length) payload
    >> log @@ "verison " ^ string_of_int  header.version 
  >>
    let txs = M.decode_block_txs payload pos in

    log @@ "txs " ^ (string_of_int <| L.length) txs
(*  >> log @@ "first tx " ^ M.formatTx (L.hd txs) *)

  >> 
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
 
  
