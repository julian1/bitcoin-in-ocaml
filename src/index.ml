
(*
	- there's a bit of duplication in that we store the lseek for 
	every output. when it only needs to be stored per hash.

	- when we index we really want to store the block pos, tx pos separately, and actual output
		separately - to get the value etc.
*)

module M = Message

type key = 
{
  hash : string ; (* tx hash *)
  index : int;
}


type value =
{
  status : string ;
  block_pos : int;
  tx_pos : int;
  tx_length : int;
	output_pos : int ; 
  output_length : int ; 
}

(* change name to encodeTXKey and decodeTXValue or IndexValue to distinguish from 
  other indexes in db.
  etc 
  --- 
  if we have several types of things - then should we be using variants? 

*)

let encodeKey (h : key ) =
(*  "/tx/" *)
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
  let pos, output_pos =  M.decodeInteger32 s pos in 
  let pos, output_length =  M.decodeInteger32 s pos in 
  { status = status; block_pos = Int64.to_int block_pos; tx_pos = tx_pos; tx_length = tx_length;
      output_pos = output_pos; output_length = output_length; 
     } 

let encodeValue (h : value)  =
  M.encodeString h.status
  ^ M.encodeInteger64 (Int64.of_int h.block_pos ) 
  ^ M.encodeInteger32 h.tx_pos 
  ^ M.encodeInteger32 h.tx_length
  ^ M.encodeInteger32 h.output_pos
  ^ M.encodeInteger32 h.output_length

let formatValue (h : value)  =
  "status " ^ h.status
  ^ " block_pos " ^ string_of_int h.block_pos 
  ^ " tx_pos " ^ string_of_int h.tx_pos 
  ^ " tx_length " ^ string_of_int h.tx_length
  ^ " output_pos " ^ string_of_int h.output_pos
  ^ " output_length " ^ string_of_int h.output_length





