
module M = Message

let (>>=) = Lwt.(>>=) 
let return = Lwt.return



type block__ =
{
    payload : string;
    hash : string;      (* avoid recalculation when store_block *)
    height : int;
    difficulty : int;  (* string ? difficulty how ? *)    
}

(* we need db to look up inputs

  let process_block x payload =
 
 *)

let validate_block db payload = 
  let _, block  = M.decodeBlock payload 0 in
  let hash = M.decode_block_hash payload in
  return ()
 


  
