
module M = Message
module U = Misc

module L = List
module CL = Core.Core_list


(*let (>>=) = Lwt.(>>=) *)
let return = Lwt.return

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
  blocks_on_request : U.SSS.t ;
  (* should change to be blocks_fd does this file descriptor even need to be here. it doesn't change?  *)
  (*  blocks_oc : Lwt_io.output Lwt_io.channel ; *)
  (* db : LevelDB.db ; *)
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
      match header.command with
        | "inv" -> (
          let _, inv = M.decodeInv payload 0 in
          (* add inventory blocks to list in peer *)
          let block_hashes = 
            inv 
            |> L.filter (fun (inv_type,hash) -> inv_type = 2 && not @@ U.SS.mem hash state.heads )
            |> L.map (fun (_,hash)->hash)
          in
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
          )
        | "block" -> (
          let hash = (M.strsub payload 0 80 |> M.sha256d |> M.strrev ) in
          let _, header = M.decodeBlock payload 0 in 
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
          let blocks_on_request = U.SSS.remove hash state.blocks_on_request in 
          { state with
              heads = heads;
              blocks_on_request = blocks_on_request;
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
      if U.SSS.is_empty state.blocks_on_request
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
          log @@ " ** requesting blocks " ^ conn.addr ^ ", head is " ^ M.hex_of_string head
           >> U.send_message conn (initial_getblocks head)
          ]
      else
        state,[]





let write_stdout = Lwt_io.write_line Lwt_io.stdout
 
let create () = 
  (* is an io function *)
  write_stdout "**** CREATE " 
  >> 
    (*  let genesis = Message.string_of_hex "000000000000000007ba2de6ea612af406f79d5b2101399145c2f3cbbb37c442" in *)
(*     let genesis = M.string_of_hex "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f" in  *)
     let genesis = M.string_of_hex "000000000000000015ca13f966458ced05d64dbaf0e4b2d8c7e35c8849c3eaec" in
      let heads =
          U.SS.empty
          |> U.SS.add genesis
           ({
            previous = "";
            height = 352775;
          } : U.my_head )   
 
    in let chain =  {
      heads = heads ;
      block_inv_pending  = None ; 
      blocks_on_request = U.SSS.empty  ;
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

