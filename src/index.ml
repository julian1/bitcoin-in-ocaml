
(*
	there's a bit of duplication in that we store the lseek for 
	every output. when it only needs to be stored per hash.
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
  lseek : int;
  length : int;
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
  let pos, lseek =  M.decodeInteger64 s pos in 
  let pos, length =  M.decodeInteger32 s pos in 
  { status = status; lseek = Int64.to_int lseek; length = length; } 

let encodeValue (h : value)  =
  M.encodeString h.status
  ^ M.encodeInteger64 (Int64.of_int h.lseek ) 
  ^ M.encodeInteger32 h.length 


