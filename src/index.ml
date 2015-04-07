
(*
	- there's a bit of duplication in that we store the lseek for 
	every output. when it only needs to be stored per hash.

	- when we index we really want to store the block pos, tx pos separately, and actual output
		separately - to get the value etc.
*)

module M = Message

type key = 
{
  hash : string ;
  index : int;
}


type value =
{
  status : string ;
  block_pos : int;
  tx_pos : int;
  tx_length : int;
}


let encodeKey (h : key ) =
  M.encodeHash32 h.hash 
  ^ M.encodeInteger32 h.index

let decodeKey s  =
  let pos, hash = M.decodeHash32 s 0 in
  let _, index = M.decodeInteger32 s pos in 
  { hash = hash; index = index } 


let decodeValue s =
  let pos, status = M.decodeString s 0 in 
  let pos, block_pos =  M.decodeInteger64 s pos in 
  let pos, tx_pos =  M.decodeInteger32 s pos in 
  let pos, tx_length =  M.decodeInteger32 s pos in 
  { status = status; block_pos = Int64.to_int block_pos; tx_pos = tx_pos; tx_length = tx_length; } 

let encodeValue (h : value)  =
  M.encodeString h.status
  ^ M.encodeInteger64 (Int64.of_int h.block_pos ) 
  ^ M.encodeInteger32 h.tx_pos 
  ^ M.encodeInteger32 h.tx_length


