
module M = Message
module U = Misc
module S = String

module L = List
module CL = Core.Core_list


(*let (>>=) = Lwt.(>>=) *)
let return = Lwt.return

let fff fd =
  Lwt_unix.unix_file_descr fd

(*
  - is there a simpler rule we could use, 
  - we only need to determine if we haven't got a block from a peer for a while...

  like if a block was just downloaded,,.
  then we remove it from the on request list.

  - why not remark all the pending with the current time ...
  - hhmmmmmm
  
  - it might be interesting to keep fd -> last mapping... around...
  - can just be a list tuple...
 
 *) 

(*
  - OK. we can create peer objects on our side, just following the Connection
  events
  - that would mean we don't even need, to pass connections
  - we can then record the last block against that peer.

  - alternatively should we use a list ... do we want to keep the ordering...
	RULE
	- if a node hasn't given us a block in a period like 10 mins...
		remove all blocks on request against that peer. 
*)

(*
	- write  block to blocks
	- then add lseek to local heads or other data structure 
	- then we can also start and stop app and load from 

	- then completely separate action, we maintain indexes..
*)

(*
  - should we pull the head structure in here?. depends are we going to export that structure
     hopefully yes... 
  - also how do we structure the next bit which involves, reading the actual blocks
    that we have...
  - we just need to scan the blocks... to load up our data structure, and then write blocks
    as we get and confirm them
  - then we have to handle tx indexing and fork arrangements... 
  - I think we need definately need add_block...

	- so we're going to have one head with the most pow. and we need to scan back to common
	fork points.
	- we can compute work here - and put it the thing.

	- new_block...
  blocks output channel
	- we also have mempool that we want to coordinate with  . 
	- VERY IMPORTANT we can post back to the main p2p message loop though...

*)

type t = { 
  heads : U.my_head U.SS.t ;
  (* change name to block_block_inv_pending *)
  block_inv_pending  : (Lwt_unix.file_descr * float ) option ; (* should include time also *) 
  (* should be a tuple with the file_desc so if it doesn't send we can clear it 
      - very important - being able to clear the connection, means we avoid
      accumulating a backlog of slow connections.
      - the test should be, if there are blocks_on_request and no block for
      x time, then wipe the connection and bloks on request.
  *)
  blocks_on_request : (Lwt_unix.file_descr * float ) U.SS.t ;
  (* should change to be blocks_fd does this file descriptor even need to be here. it doesn't change?  *)
  (*  blocks_oc : Lwt_io.output Lwt_io.channel ; *)
  (* db : LevelDB.db ; *)

  (* this would be much nicer as an indexable map *)
  last_block_received_time : (Lwt_unix.file_descr * float) list ;
}




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
  U.encodeMessage "getblocks" payload 


let initial_getdata hashes =
  (* 2 means block hashes only *)
  let encodeInventory hashes =
    let encodeInvItem hash = M.encodeInteger32 2 ^ M.encodeHash32 hash in 
      (* encodeInv - move to Message  - and need to zip *)
      M.encodeVarInt (L.length hashes )
      ^ String.concat "" @@ L.map encodeInvItem hashes 
  in
  let payload = encodeInventory hashes in 
  U.encodeMessage "getdata" payload  

(*
  - the only thing we want is a time, so if a node doesn't respond with
  an inv request, after a period we'll reissue 

  - think we might remove the fd test. 
  - the pending thing, will mean we just ignore stray blocks mostly...
  yes. a random block will be ok, since we will already have it. when synched
  and will be ignored, when blocks are on request, when not synched

  - we need to record the fd of the node that we make the block request
    to.
*)

(* let manage_chain (state : U.my_app_state ) (e : U.my_event)  =   *)

(* uggh we have to pass the conn as well - no *)

let log s = U.write_stdout s >> return U.Nop

let manage_chain1 state e    =
  match e with

    (* VERY IMPORTANT - we need to look at read errors here 
      and then reset a request, if the fd is the same. 
    *)

    | U.GotMessage (conn, header, _, payload) -> (

      let now = Unix.time () in
      match header.command with
        | "inv" -> (
          (*	- we should accept an inv that has blocks from anyone, since we may be synced and 
              anyone could have the latest mined block
            - but we use the block_inv_pending to avoid, sending requests too often when synched 
              when blocks_on_request is normally empty.
            - but only close the inv request if it's from the expected conn,
            - also append not set blocks on request

            - if someone doesn't send us a block they told us about, (eg. they disconnect) 
                it will stall in blocks_on_request, because we won't re-request it because we only 
                re-request when blocks_on_request is empty...
                unless blocks_on_request is empty...
            - if someone sends us an inv for a block that doesn't exist...
            - remember we request lots of blocks at a time...
  
            - i think the only thing we can do is record the time of the request then filter
            for being old occasionally...
            - i don't think we need the idea of a block_inv_pending... 
            the major thing is if there 
            --------------
      
            - we kind of need to check if the peer is still sending us blocks... 
            - time of last_valid_block from peer.

            - we only really need to store the fd...
            - if a peer hasn't sent us anything for a while that it said it would, then clean out the fd
            - what about a really simple rule that if a block has been on request for an hour
            we remove it...   it would actual 
            IMPORTANT - if we can close the peer connection... - then it prevents them continuing to send
              while we ignore.
            - if they don't send anything...
            - we might also remove stuff in an out of order fashion... 
          *)
          let _, inv = M.decodeInv payload 0 in
          (* add inventory blocks to list in peer *)
          let block_hashes = 
            inv 
            |> L.filter (fun (inv_type,hash) -> inv_type = 2 && not @@ U.SS.mem hash state.heads )
            |> L.map (fun (_,hash)->hash)
          in
			    if block_hashes <> [] then	
            (* clear block_inv_pending if inv came from peer against which we issued request *)
            let block_inv_pending = match state.block_inv_pending with 
              | Some (fd, _) when fd == conn.fd -> None
              | a -> a
            in 
            (* add to blocks on request, and request them from the peer *)
            let blocks_on_request =
              (* Note, we will latest record if request from multiple peers *)
              L.fold_left (fun m h -> U.SS.add h (conn.fd, now) m) state.blocks_on_request  block_hashes	
            in
(*
            let blocks_on_request = (*U.SS.union state.blocks_on_request*) (U.SS.of_list block_hashes)
*)
            ( { state with
                block_inv_pending = block_inv_pending;
                blocks_on_request = blocks_on_request ;
              }, 
              [
                log @@ U.format_addr conn ^ " *** got block inv "
                  ^ string_of_int @@ List.length block_hashes;
                  U.send_message conn (initial_getdata block_hashes );
              ] 
            )
          else
            (state, [] )

       (* 
          match state.block_inv_pending with 
            | Some (fd, _) when fd == conn.fd && not (CL.is_empty block_hashes ) -> 
              (* probably in response to a getdata request *)
              (* let h = CL.take block_hashes 10 in *)
              let h = block_hashes in
              ( { state with
                (* can we already have blocks on request ? *) 
                blocks_on_request = U.SSS.of_list h;
                block_inv_pending = None;
              },
                 [
                  log @@ U.format_addr conn ^ " *** WHOOT chainstate got inv "
                  ^ string_of_int @@ List.length block_hashes;
                  U.send_message conn (initial_getdata h );
                ] )
            | _ ->  (state, [] )
        *)
          )
        | "block" -> (
          let hash = (M.strsub payload 0 80 |> M.sha256d |> M.strrev ) in
          let _, header = M.decodeBlock payload 0 in 

          (* update the last received block against the fd *)
          let last = L.filter (fun (fd,t) -> fd != conn.fd) state.last_block_received_time in
          let last = (conn.fd, now ):: last
          in

          (* if don't have block, but links into sequence then include *)
          let heads, height =
            if not (U.SS.mem hash state.heads ) && (U.SS.mem header.previous state.heads) then 
                let height = (U.SS.find header.previous state.heads) .height + 1 in
                U.SS.add hash ( { 
                  previous = header.previous;  
                  height =  height; 
                } : U.my_head )  state.heads, height
            else
              state.heads, -999
          in
          let blocks_on_request = U.SS.remove hash state.blocks_on_request in 
          { state with
              heads = heads;
              blocks_on_request = blocks_on_request;
              last_block_received_time = last;
             (* jobs = state.jobs @  *)
          },
          [ log @@ U.format_addr conn ^ " block " ^ M.hex_of_string hash ^ " " ^ string_of_int height; 
         ]
        ) 
		    | _ -> state, []
	   ) 
    | _ -> state, []




let manage_chain2 state connections  e   =
  (* make requests for block inv *)
  match e with
    | U.Nop -> state, []
    | _ ->
      (* we need to check we have completed handshake *)
      (* shouldn't we always issue a request when blocks_on_request *) 
      let now = Unix.time () in
      (* clear a pending inv request, if peer never responded, 
        - should also close peer?  *) 
      let state = 
        match state.block_inv_pending with 
          | Some (_, t) when now > t +. 60. ->  
            { state with block_inv_pending = None }
          | _ -> state
      in 
      (* - if no blocks on request, and have connections, and no inv pending
        then do an inv request *)
      if U.SS.is_empty state.blocks_on_request
        && not (CL.is_empty connections)
        && state.block_inv_pending = None then

        (* create a set of all pointed-to block hashes *)
        (* watch out for non-tail call optimised functions here which might blow stack  *)
        let previous =
          U.SS.bindings state.heads
          |> List.rev_map (fun (_, (head : U.my_head) ) -> head.previous)
          |> U.SSS.of_list
        in
        (* get the tips of the blockchain tree by filtering all block hashes against the set *)
        let heads =
          U.SS.filter (fun hash _ -> not @@ U.SSS.mem hash previous ) state.heads
          |> U.SS.bindings
          |> List.rev_map (fun (tip,_ ) -> tip)
        in
        (* choose a tip at random *)
        let index = now |> int_of_float |> (fun x -> x mod List.length heads) in
        let head = List.nth heads index in

        (* choose a conn at random *)
        let index = now |> int_of_float |> (fun x -> x mod List.length connections ) in
        let (conn : U.connection) = List.nth connections index in

        (* we need to record if handshake has been performed
          - which means a peer structure
          - we should be recording peer version anyway.
        *)
        { state with
          block_inv_pending = Some (conn.fd, now ) ;
        },
        [
          log @@ S.concat "" [ 
            "request addr " ; conn.addr; 
            "\nblocks on request " ; string_of_int (U.SS.cardinal state.blocks_on_request) ; 
            "\nheads count " ; string_of_int (L.length heads);
            "\nrequested head is ";  M.hex_of_string head
          ]; 
           U.send_message conn (initial_getblocks head)
          ]
      else
        state,[]





let write_stdout = Lwt_io.write_line Lwt_io.stdout
 
let create () = 
  (* is an io function *)
  write_stdout "**** CREATE " 
  >> 
      let heads1 =
          U.SS.empty
          |> U.SS.add (M.string_of_hex "000000000000000015ca13f966458ced05d64dbaf0e4b2d8c7e35c8849c3eaec") 
           ({
            previous = "";
             height = 352775; 
          } : U.my_head )   
    	in
      let heads2 =
          U.SS.empty
          |> U.SS.add (M.string_of_hex "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f") 
           ({
            previous = "";
            height = 0;
          } : U.my_head )   
 
 
    in let chain =  {
      heads = heads2 ;
      block_inv_pending  = None ; 
      blocks_on_request = U.SS.empty  ;
    last_block_received_time = [];
  } in
  (return chain)


(*
  how, why are these jobs running? 
  VERY IMPORTANT - perhaps we need to add a (), no i think the job is scheduled 
*)

let update state connections e  = 
  let state, jobs1 = manage_chain1 state e  in
  let state, jobs2 = manage_chain2 state connections e in
  state, jobs1 @ jobs2

