

(* ok this thing needs access to some of the shared data structures
namely my_event 

Actually we ought to be able to have the chainstate structure be completely 
  shielded here.  connections should be shielded... but jobs need to be shared 
*)

open Misc

module L = List
module CL = Core.Core_list




(* let manage_chain (state : Misc.my_app_state ) (e : Misc.my_event)  =   *)
 let manage_chain state  e   =  

  match e with
    
    | GotMessage (conn, header, raw_header, payload) -> 
	(
      match header.command with
        | "inv" ->
          let _, inv = Message.decodeInv payload 0 in
          (* add inventory blocks to list in peer *)
          let needed_inv_type = 2 in
          let block_hashes = inv
          |> L.filter (fun (inv_type,_) -> inv_type = needed_inv_type )
          |> L.map (fun (_,hash)->hash)
          (* ignore blocks we already have *)
          |> L.filter (fun hash -> not @@ SS.mem hash state.heads )
          in

          { state with 
            jobs = state.jobs @ [
              log @@ format_addr conn ^ " chainstate got inv " 
                ^ string_of_int @@ List.length block_hashes
            ]
          }
		| _ -> 
        (* we need to check we have completed handshake *)
        let now = Unix.time () in
 
        if CL.is_empty state.requested_blocks  
            && now > state.time_of_last_received_block +. 180. then 
          state
        else
          state
		
		)
    | _ -> state

(* we've got an issue about adding read jobs - can only do it once

	IMPORTANT we may want to a flag in state structure to indicate whether the message 
	has been handled...
 *)

