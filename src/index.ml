
module M = Message

type value =
{
  status : string ;
  lseek : int;
}

type key = 
{
  hash : string ;
  index : int;
}



let decodeValue s =
  let pos, status = M.decodeString s 0 in 
  let pos, lseek =  M.decodeInteger64 s pos in 
  { status = status; lseek = Int64.to_int lseek; } 

let encodeValue (h : value)  =
  M.encodeString h.status
  ^ M.encodeInteger64 (Int64.of_int h.lseek ) 


let encodeKey (h : key ) =
  M.encodeHash32 h.hash 
  ^ M.encodeInteger32 h.index

let decodeKey s  =
  let pos, hash = M.decodeHash32 s 0 in
  let _, index = M.decodeInteger32 s, pos in 
  { hash = hash; index = index } 




