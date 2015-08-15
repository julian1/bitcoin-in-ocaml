
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
*)

let validate_block db payload = 
    let hash = M.decode_block_hash payload in
    log @@ "hash " ^ M.hex_of_string hash
  >>
    let pos, header = M.decodeBlock payload 0 in
    log @@ "payload length " ^ (string_of_int <| S.length) payload
  >> 
    let pos, aux_tx = M.decodeTx payload 80 in 
    log @@ "auxpow " ^ M.formatTx aux_tx
  >>
	  let pos, aux_block_hash = M.decodeHash32 payload pos in
    log @@ "aux_block_hash " ^ M.hex_of_string aux_block_hash
  >>
    let pos, branch_length1 = M.decodeVarInt payload pos in
    log @@ "branch length1 " ^ string_of_int branch_length1

  >> let pos = pos + (branch_length1 * 32) + 4 in
    let pos, branch_length2 = M.decodeVarInt payload pos in
    log @@ "branch length2 " ^ string_of_int branch_length2

  >>
    let pos = pos + (branch_length2 * 32) + 4 in
    
    let pos = pos + 80 in 
    log @@ "hex " ^ M.hex_of_string (M.strsub payload pos (S.length payload - pos)) 
    (*  >> log @@ "hex " ^ M.hex_of_string payload *)
  >>
  let txs = M.decode_block_txs payload in
  log @@ "txs " ^ (string_of_int <| L.length) txs
  >> log @@ "first tx " ^ M.formatTx (L.hd txs)

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
 
  
