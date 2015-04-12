
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

(* change name to encodeTXOKey and decodeTXOValue to distinguish from 
  other indexes in db.
  etc 
  --- 
  if we have several types of things - then should we be using variants? 

  encodeTXOKey
*)

let decodeKey s  =
  let pos = 1 in
  let pos, hash = M.decodeHash32 s pos in
  let _, index = M.decodeInteger32 s pos in 
  { hash = hash; index = index } 

let encodeKey (h : key ) =
  "t"
  ^ M.encodeHash32 h.hash 
  ^ M.encodeInteger32 h.index


(* - we should use hash 160 if we can, to be independent of implementation, main-net test-net etc. 
    - although that will lose the ability to lookup a pubkey...

    - but if we store the raw bytes,
    - then we can't just lookup everything for an address - uggh

    pubkey -> 160, then can find all addresses.

    we might have a separate index for pubkeys.
    -------
    IMPORTANT,
      - why not just index the entire output script? rather than just the bytes...

    - to determine the value in an address. need to see what outputs can spend, means
      OP_DUP types 
      CHECKSIG types.
  
    - as another alternative, should we just index the output script 
    - then we can determine irrespective of if we use ...
      - by just constructing the query in different ways...
      - but we 
      - can lookup an address (no can't) we can scan arbitrarily...

    - do we really even care about addresses? unless it's for us to spend.  

    REMEMBER ALSO, 
      - we still have to add the txo stuff...

    VERY IMPORTANT
      - we need to be able to unwind the action, on chain-state reorganisation. 
      no, this is just whether the output is spent, which we avoid recording.
*) 

let encodeAddressKey (h : key ) =
  "a"
  ^ M.encodeString "blah"
  ^ M.encodeHash32 h.hash 
  ^ M.encodeInteger32 h.index





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


