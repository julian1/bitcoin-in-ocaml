
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
  Rule, for removing. 


  Hang on. 
    Can't we do it by looking at the minimum time a block has been in the requested blocks. 
    if anything has been sitting there.

--------
  - in time on request > 10 mins && last block send > 10mins 
  
    - if we requested a block at least 10 minutes ago, and haven't received anything for 10 mins 

    ahh. no. if time requested from minimum in - last block sent, then clear everythgin...
    - makes it easy...

    - so map blocks_on_request and get the smallest time for each fd. 
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

  (* the hashes *)
  heads : U.my_head U.SS.t ;

  (* when we issue an inv request against a peer *) 
  block_inv_pending  : (Lwt_unix.file_descr * float ) option ; (* should include time also *) 
  (* should be a tuple with the file_desc so if it doesn't send we can clear it 
      - very important - being able to clear the connection, means we avoid
      accumulating a backlog of slow connections.
  *)
  blocks_on_request : (Lwt_unix.file_descr * float ) U.SS.t ;

  (* this would be nicer as an indexable map - we can record any peer stats we like here 
    to help us. we could also use this as the source of connections against which to do requests
  *)
  last_block_received_time : (Lwt_unix.file_descr * float) list ;

  (* should change to be blocks_fd does this file descriptor even need to be here. it doesn't change?  *)
  (*  blocks_oc : Lwt_io.output Lwt_io.channel ; *)
  (* db : LevelDB.db ; *)
}

(*
  - we only care about the minimum time, fd 
  map -> list...
  - then we'll walk the list....
  - uggh actually it won't work...
  - if we requested the blocks 20 minutes ago, then they'll all show an age of 20 minutes...
  because its when the block was cleared...  no doesn't matter. 
  - if we know that any block has been on request greater than 10 minutes from a conn, but
  we haven't received anything from that conn, then we should clear.
  - ok, rather than organizing by block.  should we organize by fd?

  -----
  - if we get nothing for 3 minutes it's a read error.
  - what about if we get no block for 3 minutes then we generate a read error and clear...

  - when do request for a block we'll add to the last_block_received_time...
    if that exceeds 3 minutes then we'll clear blocks_on_request...

  we have to prevent a newly put block from triggering...


*)

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
          (* extract inventory blocks we don't know about *)
          let block_hashes = 
            inv 
            |> L.filter (fun (inv_type,hash) -> inv_type = 2 
              && not (U.SS.mem hash state.heads ) 
              && not ( U.SS.mem hash state.blocks_on_request)  )
            |> L.map (fun (_,hash)->hash)

            (* should also filter against blocks that are on request *)
          in
			    if block_hashes <> [] then	
            (* clear block_inv_pending if inv came from peer against which we issued request *)
            let block_inv_pending = match state.block_inv_pending with 
              | Some (fd, _) when fd == conn.fd -> None
              | a -> a
            in 
            (* add to the blocks on request, and request them from the peer *)
            let blocks_on_request =
              (* Note, we will latest record if request from multiple peers *)
              L.fold_left (fun m h -> U.SS.add h (conn.fd, now) m) state.blocks_on_request  block_hashes	
            in
            ( { state with
                block_inv_pending = block_inv_pending;
                blocks_on_request = blocks_on_request ;
              }, 
              [
                log @@ U.format_addr conn ^ " *** got block inv requesting block/s "
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
              state.heads, -999 (* should be None *)
          in
          let blocks_on_request = U.SS.remove hash state.blocks_on_request in 
          { state with
              heads = heads;
              blocks_on_request = blocks_on_request;
              last_block_received_time = last;
             (* jobs = state.jobs @  *)
          },
          [ log @@ U.format_addr conn ^ " block " ^ M.hex_of_string hash ^ " " ^ string_of_int height
            ^ " on request " ^ string_of_int @@ U.SS.cardinal blocks_on_request ; 
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

      (* 
        ok, if someone sends us random invs, then they'll sit in blocks on request 
        and basically prevent us downloading...

        get descriptors that we haven't received valid blocks for a while 
        we don't want to be waking over the full set of descriptors every time ...
        filter and map can be replaced with a single fold...
      *)
(*      let fds = L.filter (fun (fd,t) -> now > t +. 60.) state.last_block_received_time 
                |> L.map (fun (fd,t) -> fd )
      in
 *) 
      let blocks_on_request = U.SS.filter (fun hash (fd,t) -> 
        not ( now > t +. 60. 
          && L.exists (fun (fd_,t_) -> fd_ == fd && now > t_ +. 60.) state.last_block_received_time 
          )  (* we need an exists using physical equality *) 
        ) state.blocks_on_request in 
          


      (* - if no blocks on request, and have connections, and no inv pending
        then do an inv request *)
      if U.SS.is_empty blocks_on_request
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
          blocks_on_request = blocks_on_request;
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
        { state with 
          blocks_on_request = blocks_on_request;
        } ,[]





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

