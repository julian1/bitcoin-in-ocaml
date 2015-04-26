
module M = Message
module U = Misc
module S = String

module L = List
module CL = Core.Core_list


let (>>=) = Lwt.(>>=) 
let return = Lwt.return

(*
let fff fd =
  Lwt_unix.unix_file_descr fd
*)

(*
  - ok, now we need more block rules (merckle root, difficulty, time checks )
  - then we need to save...

  - issue is that saving is an async action.  that's ok. 
    we ought to be able to save, then update the position...

  - can we factor o 
*)

(*
	- write block to disk
	- add lseek to local heads or other data structure
	- then we can also start and stop app and load heads from disk
	- then completely separate action, we maintain indexes..
*)


type ggg = {
  fd : Lwt_unix.file_descr ;
  t : float ;
}


type t = {

  (* tree structure *)
  heads : U.my_head U.SS.t ;

  (* set when inv request made to peer *)
  block_inv_pending  : (Lwt_unix.file_descr * float ) option ;

  (* blocks peer, time   *)
  blocks_on_request : (Lwt_unix.file_descr * float ) U.SS.t ;

  (*   *)
(*  last_block_received_time : (Lwt_unix.file_descr * float) list ;
*)

  last_block_received_time : ggg list ;


  (* should change to be blocks_fd does this file descriptor even need to be here. it doesn't change?  *)
  (*  blocks_oc : Lwt_io.output Lwt_io.channel ; *)
  (* db : LevelDB.db ; *)

  blocks_fd_m :   Lwt_mutex.t ;

  blocks_fd : Lwt_unix.file_descr ;
 

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


let log s = U.write_stdout s >> return U.Nop

let manage_chain1 state e    =
  match e with

    (* TODO connection errors should monitor read errors and clear fd *)
    | U.GotMessage (conn, header, _, payload) -> (

      let now = Unix.time () in
      match header.command with
        | "inv" -> (
          (*	- we accept a block inventory from any peer, since if synced any peer could 
            have the latest mined block first
            - but we prioritize solicited over unsolicted inventory  
            - also block_inv_pending is used to avoid sending requests too often when synched
          *)
          (* extract blocks from inventory message, and filter for those we don't know *)
          let _, inv = M.decodeInv payload 0 in
          let block_hashes =
            inv
            |> L.filter (fun (inv_type,hash) ->
              inv_type = 2
              && not (U.SS.mem hash state.heads )
              && not ( U.SS.mem hash state.blocks_on_request)  )
            |> L.map (fun (_,hash) -> hash)
          in
          (* prioritize handling *)
          let block_hashes =
              match state.block_inv_pending with
              (* take all the blocks if solicited *)
              | Some (fd, _) when fd == conn.fd -> block_hashes
              (* discard if already have blocks on request from the same peer *)
              | _ ->
                if U.SS.exists (fun _ (fd,_) -> fd == conn.fd) state.blocks_on_request then
                  []
                else
                  block_hashes
          in
          (* blocks we want *)
          if block_hashes <> [] then
            (* maybe clear block_inv_pending *)
            let block_inv_pending = match state.block_inv_pending with
              | Some (fd, _) when fd == conn.fd -> None
              | a -> a
            in
            (* record in blocks now on request *)
            let blocks_on_request =
              L.fold_left (fun m h -> U.SS.add h (conn.fd, now) m) state.blocks_on_request block_hashes
            in
            ( { state with
                block_inv_pending = block_inv_pending;
                blocks_on_request = blocks_on_request ;
              },
              [
                log @@ U.format_addr conn
                  ^ " *** got inventory blocks " ^ (string_of_int @@ L.length block_hashes  )
                  ^ " - on request " ^ string_of_int @@ U.SS.cardinal blocks_on_request ;
                  (* request blocks from the peer *)
                  U.send_message conn (initial_getdata block_hashes );
              ]
            )
          else
            (state, [] )
          )

        | "block" -> (
          (* we received a block *)
          let hash = (M.strsub payload 0 80 |> M.sha256d |> M.strrev ) in
          let _, header = M.decodeBlock payload 0 in

          (* if we don't yet have the block, and it links into chain then include 
              ok, we are committed to writing the block to disk and including, 
              then lets do the io, first not screate another message
          *)
          let heads, height =
            if not (U.SS.mem hash state.heads ) && (U.SS.mem header.previous state.heads) then
                let height = (U.SS.find header.previous state.heads).height + 1 in
                U.SS.add hash ( {
                  previous = header.previous;
                  height =  height;
                } : U.my_head )  state.heads, height
            else
              state.heads, -1 (* should be None *)
          in
          (* update the time that we got a valid block from the peer *)
          let last =
            if height <> -1 then
              (* update the fd to indicate we got a good block, TODO tidy this *)
              let last = L.filter (fun x -> x.fd != conn.fd) state.last_block_received_time in
              { fd = conn.fd; t = now ;
              }::last
            else
              state.last_block_received_time
          in
          (* remove from blocks on request *)
          let blocks_on_request = U.SS.remove hash state.blocks_on_request in
          { state with
              heads = heads;
              blocks_on_request = blocks_on_request;
              last_block_received_time = last;
          },
          [ log @@ U.format_addr conn ^ " block " ^ M.hex_of_string hash ^ " " ^ string_of_int height
            ^ " on request " ^ string_of_int @@ U.SS.cardinal blocks_on_request ;

            let s = "whoot\n" in 
            (* write fd buf ofs len has the same semantic as Unix.write, but is cooperative 
                how do we read this block data again. fd though???
                we're going to have to open the file everytime if we want to be able to lseek the end ...

                - Important rather than spinning through a message....

                - seems that we're not positioned at the end initially...

                  - ughhh, we can't just query the position, and then do a separate function....
                  because 
                  - we need a queue or the ability to sequence the writes....
                  - otherwise recording the position isn't guaranteed...

                23.227.191.50:8333  block 000000000000000007bba9bd66a0198babed0334539318369102c30223008d89 353727 on request 25
                23.227.191.50:8333  block 000000000000000000408a768f84c20967fac5c12cc6ed00717b19364997eeba 353728 on request 24
                 pos 30
                 pos 30

                - we can seek the end when we first open the file...
				- but what about doing the writing, we're going to have to create an effective mutex...
				- to synchronize.
				- so much fucking io.
					-----

				- IMPORTANT - OK we just use a lwt mutex actually lwt may have mutexes...

				we could return an array that would be 
					
				- also in the time that we're trying to write it, we can't clear from blocks_on_request
				- fuck 	
            *)
			(*
				rather than option monads everywhere, actions ought to take two functions 
					the intital and the failure (usually log)   problem is the creation functions.

        OK, because we're not initially moved to the end of the file, until after we try to write.
        it might be better to write the file, then substract the length...
			*)
            Lwt_mutex.lock state.blocks_fd_m
            >> Lwt_unix.write state.blocks_fd s 0 (S.length s) 
            >>= fun count -> 
            Lwt_unix.lseek state.blocks_fd 0 Unix.SEEK_CUR
            >>= fun pos -> let () = Lwt_mutex.unlock state.blocks_fd_m in
            log @@  " pos " ^ string_of_int (pos - count) 

         ]
        )
        | _ -> state, []
      )
    | _ -> state, []


let manage_chain2 state connections  e   =
  (* issue inventory requests to advance the tips *)
  match e with
    | U.Nop -> state, []
    | _ ->
      (* we need to check we have completed handshake *)
      (* shouldn't we always issue a request when blocks_on_request *)
      let now = Unix.time () in

      (* if peer never responded to an inv, clear the pending flag *)
      let state =
        match state.block_inv_pending with
          | Some (_, t) when now > t +. 60. ->
            { state with block_inv_pending = None }
          | _ -> state
      in

      (* if someone sends us lots random invs, then clog up blocks_on_request
        and prevent us issuing inv from the current tips...  this is an issue. *)

      (* if a block was requested at least 60 seconds ago, and
      we haven't received any valid blocks from the corresponding peer for at least 60 seconds, then
      clear from blocks_on_request to permit re-request from a different peer *)
      let state = { state with
        blocks_on_request = U.SS.filter (fun hash (fd,t) ->
          not (
            now > t +. 60.
            && match CL.find state.last_block_received_time (fun x -> x.fd == fd) with
              | Some x -> now > x.t +. 60.
              | None -> true
            )
          ) state.blocks_on_request
      } in

      (* if there are no blocks on request, and no inv pending then do an inv request *)
      if U.SS.is_empty state.blocks_on_request
        && state.block_inv_pending = None 
        && not (CL.is_empty connections) then

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

        (* choose a peer fd at random *)
        let index = now |> int_of_float |> (fun x -> x mod List.length connections ) in
        let (conn : U.connection) = List.nth connections index in

        (* TODO we need to record if handshake has been performed *)
        { state with
          block_inv_pending = Some (conn.fd, now ) ;
        },
        [
          log @@ S.concat "" [
            "request addr " ; conn.addr;
            "\nblocks on request " ; string_of_int (U.SS.cardinal state.blocks_on_request) ;
            "\nheads count " ; string_of_int (L.length heads);
            "\nrequested head is ";  M.hex_of_string head ;

            "\n fds\n" ; S.concat "\n" ( L.map (fun x -> string_of_float (now -. x.t ) ) state.last_block_received_time )
          ];
          (* request inv *)
           U.send_message conn (initial_getblocks head)
          ]
      else
        state,[]



let write_stdout = Lwt_io.write_line Lwt_io.stdout

let create () =
  (* initialization should be an io function? 
    
    how can we initialize if the fd failed????

    we could use continuation passing style, so if the action failed, 
      we could output the error
      we kind of also need to catch file exceptions.
   *)
  write_stdout "**** initializing chain "
 

 
  (* open blocks.dat writer *)
  >> Lwt_unix.openfile "blocks.dat"  [O_WRONLY ; O_APPEND ; O_CREAT ] 0o644 
  >>= fun blocks_fd -> 
      match Lwt_unix.state blocks_fd with
        | Opened -> ( 
          let heads1 =
              U.SS.empty
              |> U.SS.add (M.string_of_hex "000000000000000011351a1fcb93ffd9b20eeaca87c78ef7df87153a2237b91f")
               ({
                previous = "";
                 height = 353721;
              } : U.my_head )
          in
          let heads2 =
              U.SS.empty
              |> U.SS.add (M.string_of_hex "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f")
               ({
                previous = "";
                height = 0;
              } : U.my_head )
          in 
          let chain =  {
            heads = heads1 ;
            block_inv_pending  = None ;
            blocks_on_request = U.SS.empty  ;
            last_block_received_time = [];
            blocks_fd_m = Lwt_mutex.create (); 

            blocks_fd = blocks_fd ;
        } in
        return (Some chain)
        ) 
      | _ -> return None

(* there's an issue that jobs are running immediately before placing in choose() ?  *)

let update state connections e  =
  let state, jobs1 = manage_chain1 state e  in
  let state, jobs2 = manage_chain2 state connections e in
  state, jobs1 @ jobs2




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

(*
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

