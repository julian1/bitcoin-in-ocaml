

(* ok this thing needs access to some of the shared data structures
namely my_event 

Actually we ought to be able to have the chainstate structure be completely 
  shielded here.  connections should be shielded... but jobs need to be shared 
*)

(*
  - important - it looks like this code, will just ignore random blocks that
  inv advertises us. which is really nice.
  - because we analyze everything that we request 
  ---  
  when we make an inv request we do it against a specific node, we can 
  therefore know, what we expect from that node. and can ignore other inv 
  while request is not empty.
  - inv and request is empty and from specific address, then probably
    in response to a get data 
*)

open Misc

module M = Message
module L = List
module CL = Core.Core_list



let initial_getblocks starting_hash =
  (* the list are the options, and peer will return a sequence
    from the first valid block in our list *)
  let payload =
    M.encodeInteger32 1  (* version *)
    ^ M.encodeVarInt 1
    ^ M.encodeHash32 starting_hash
                        (* TODO should be list of hashes *)
    ^ M.zeros 32   (* block to stop - we don't know should be 32 bytes *)
  in
  let header = M.encodeHeader {
    magic = Misc.magic ;
    command = "getblocks";
    length = M.strlen payload;
    checksum = M.checksum payload;
  } in
  header ^ payload



(* let manage_chain (state : Misc.my_app_state ) (e : Misc.my_event)  =   *)
let manage_chain1 state  e   =  

  match e with
    | GotMessage (conn, header, raw_header, payload) -> 
	    (
      match header.command with
        | "inv" ->
          let _, inv = M.decodeInv payload 0 in
          (* add inventory blocks to list in peer *)
          let needed_inv_type = 2 in
          let block_hashes = inv
          |> L.filter (fun (inv_type,_) -> inv_type = needed_inv_type )
          |> L.map (fun (_,hash)->hash)
          (* ignore blocks we already have *)
          |> L.filter (fun hash -> not @@ SS.mem hash state.heads )
          (* should ignore pending also *)
          in
          { state with 
            jobs = state.jobs @ [
              log @@ format_addr conn ^ " chainstate got inv " 
                ^ string_of_int @@ List.length block_hashes
            ]
          }
		    | _ -> state	
		  )
    | _ -> state


let manage_chain2 state  e   =  
  match e with
    | Nop -> state 
    | _ ->
      (* we need to check we have completed handshake *)
      let now = Unix.time () in
      if CL.is_empty state.requested_blocks  
        && now > state.time_of_last_received_block +. 180. then 

        (* create a set of all pointed-to block hashes *)
        (* watch out for non-tail call optimised functions here which might blow stack  *)
        let previous = 
          SS.bindings state.heads 
          |> List.rev_map (fun (_,head ) -> head.previous) 
          |> SSS.of_list
        in
        (* get the tips of the blockchain tree by filtering all block hashes against the set *)
        let heads = 
          SS.filter (fun hash _ -> not @@ SSS.mem hash previous ) state.heads 
          |> SS.bindings 
          |> List.rev_map (fun (tip,_ ) -> tip) 
        in
        (* choose a head at random *)
        let index = now |> int_of_float |> (fun x -> x mod List.length heads) in 
        let head = List.nth heads index in

        (* we need to record if handshake has been performed 
          - which means a peer structure *)
(*   >> send_message conn (initial_getblocks head) *)
 
        { state with 
          time_of_last_received_block = now; 
          jobs = state.jobs @ [
            log @@ " need to request some blocks head is " ^ M.hex_of_string head 
          (* >> send_message conn (initial_getblocks head) *)
 
          ]
        }
      else
        state



let manage_chain state e   =  
  let state = manage_chain1 state  e in 
  manage_chain2 state  e






(* we've got an issue about adding read jobs - can only do it once

	IMPORTANT we may want to a flag in state structure to indicate whether the message 
	has been handled...
 *)

