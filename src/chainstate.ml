

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
(*
  - works really nice to download, since it just ignores everything unless its
  a response in relation to a request
  - but won't be so good in downloading from random node,  

*)
open Misc

module M = Message
module L = List
module CL = Core.Core_list



let initial_getblocks starting_hash =
  (* the list are the options, and peer will return a sequence
    from the first valid block in our list *)
  (* TODO should be list of hashes *)
  let payload =
    M.encodeInteger32 1  (* version *)
    ^ M.encodeVarInt 1
    ^ M.encodeHash32 starting_hash
    ^ M.zeros 32   (* block to stop - we don't know should be 32 bytes *)
  in
  encodeMessage "getblocks" payload 


let initial_getdata hashes =
  (* 2 means block hashes only *)
  let encodeInventory hashes =
    let encodeInvItem hash = M.encodeInteger32 2 ^ M.encodeHash32 hash in 
      (* encodeInv - move to Message  - and need to zip *)
      M.encodeVarInt (L.length hashes )
      ^ String.concat "" @@ L.map encodeInvItem hashes 
  in
  let payload = encodeInventory hashes in 
  encodeMessage "getdata" payload  

(*
  - the only thing we want is a time, so if a node doesn't respond with
  an inv request, after a period we'll reissue 

  - think we might remove the fd test. 
  - the pending thing, will mean we just ignore stray blocks mostly...
  yes. a random block will be ok, since we will already have it. when synched
  and will be ignored, when blocks are on request, when not synched
*)

(* let manage_chain (state : Misc.my_app_state ) (e : Misc.my_event)  =   *)
let manage_chain1 state  e   =

  match e with
    | GotMessage (conn, header, raw_header, payload) -> (
      match header.command with
        (* ok there's an issue that any inv is meaning we cancel the block request
        then it doesn't match the fd *)

        | "inv" -> (
          let _, inv = M.decodeInv payload 0 in
          (* add inventory blocks to list in peer *)
          let block_hashes = 
            inv 
            |> L.filter (fun (inv_type,hash) -> inv_type = 2 && not @@ SS.mem hash state.heads )
            |> L.map (fun (_,hash)->hash)
          in
          match state.inv_pending with 
            | Some (fd, t) when fd == conn.fd && not (CL.is_empty block_hashes ) -> 
              (* probably in response to a getdata request *)
              (* let h = CL.take block_hashes 10 in *)
              let h = block_hashes in
              { state with
                (* can we already have blocks on request ? *) 
                blocks_on_request = SSS.of_list h;
                inv_pending = None;
                jobs = state.jobs @ [
                  log @@ format_addr conn ^ " *** WHOOT chainstate got inv "
                  ^ string_of_int @@ List.length block_hashes;
                  send_message conn (initial_getdata h );
                ]
                }
            | _ ->  state
          )

        | "block" -> (
          let hash = (M.strsub payload 0 80 |> M.sha256d |> M.strrev ) in
          let _, header = M.decodeBlock payload 0 in 
  
          (* if don't have block, but links into sequence then include *)
          let heads, height =
            if not (SS.mem hash state.heads ) && (SS.mem header.previous state.heads) then 
                let height = (SS.find header.previous state.heads) .height + 1 in
                SS.add hash { 
                  previous = header.previous;  
                  height =  height; 
                } state.heads, height
            else
              state.heads, -999
          in
          let blocks_on_request = SSS.remove hash state.blocks_on_request in 
          { state with
              heads = heads;
              blocks_on_request = blocks_on_request;
              jobs = state.jobs @ [
                log @@ format_addr conn ^ " block " ^ M.hex_of_string hash ^ " " ^ string_of_int height; 
              ]
          }

        ) 
		    | _ -> state
		  )
    | _ -> state


let manage_chain2 state  e   =
  match e with
    | Nop -> state
    | _ ->
      (* we need to check we have completed handshake *)
      (* shouldn't we always issue a request when blocks_on_request *) 
      let now = Unix.time () in
      (* clear a pending inv request, if peer never responded, 
        should also discard peer? *) 
      let state = 
        match state.inv_pending with 
          | Some (fd, t) when now > t +. 60. ->  
            { state with inv_pending = None }
          | _ -> state
      in 
      (*
        if no blocks on request, and have connections, and no inv pending
        then do an inv request
      *)
      if SSS.is_empty state.blocks_on_request
        && not (CL.is_empty state.connections)
        && state.inv_pending = None then

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
        (* choose a tip at random *)
        let index = now |> int_of_float |> (fun x -> x mod List.length heads) in
        let head = List.nth heads index in

        (* choose a conn at random *)
        let index = now |> int_of_float |> (fun x -> x mod List.length state.connections ) in
        let conn = List.nth state.connections index in

        (* we need to record if handshake has been performed
          - which means a peer structure
          - we should be recording peer version anyway.
        *)
        { state with
          inv_pending = Some (conn.fd, now ) ;
          jobs = state.jobs @ [
            log @@ " ** requesting blocks " ^ conn.addr ^ ", head is " ^ M.hex_of_string head
           >> send_message conn (initial_getblocks head)
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

