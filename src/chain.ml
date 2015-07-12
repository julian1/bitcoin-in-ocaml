
module M = Message
module U = Misc
module S = String

module L = List
module CL = Core.Core_list



let (>>=) = Lwt.(>>=) 
let return = Lwt.return


(*
  - ok, now we need more block rules (merckle root, difficulty, time checks )
  - then we need to save...

    RIGHT - the jobs are actually jobs in an array... 
            are we sure we don't want 
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
      ^ S.concat "" @@ L.map encodeInvItem hashes
  in
  let payload = encodeInventory hashes in
  U.encodeMessage "getdata" payload


let log s = U.write_stdout s >> return U.Nop

(*
  - ok, the inventory stuff wants to be sequenced as well... so it uses the db connection only once.
  - we have to lookup blocks in db, to ensure we don't already have them... before requesting

  - the problem is the state updating...
  - we don't have access to it. 
  - VERY IMPORTANT could we have a set of variables that get altered only in the context of synch
  - process???   then we could bind the state in and return the new state on the output? 

  - in fact why not sequence all events on the same queue??? because it would make everything
    io orientated...

  - we could immediately create the a GotInveinve

   - so we can do whatever the hell io operations we like. 
    rather than have io operations in the state.jobs   

  - before the sequence job is run, the state should be injected into it.
  ------------
  this also means that we don't actually need to put the blocks on request as complete
  things in the sequence job queue, 


  a job that's complete, get's it's result put in the queue, except if it's a nop or finish seq job. 

  so there's an issue - that a queue will lag, and resources like fd may have closed ...
                        won't get reflected.
                      - we could insert at the front of the queue.

  - need to filter blocks. most stuff can done in memory. would it be easier to structure...
  - there's only one db conn, so 

  - Continuation f 
  - so an Continuation f, can just have it's action f called with state... 
    - but this doesn't solve block lookup, which has to go through the sequencer... 

  ------------------------------------- 
  - two ways to solve
    - make everything io based, around a queue. pushing computations back and front.
        adv - we want to do io everywhere - eg. lookup old connections etc.
            - might be simpler. 
    - make everything pure, with sequence points for state, and queues.  
   
  - it's getting complicated... with the jobs in the state and the queue.   

  - with the serializer queue, we can actually hold state object through the action...
  ----

    does it need to be so complicated. 
      - compute as is.
      - schedule an io job in jobs
      - schedule to run in seq jobs if uses the db resource.

    Ughhh. this is messy. even though we need a sequence for db, we have to update the record blocks on request which
      means changing the state. which we can't do inside the sequence...

    FUCK... there's memory state and db state that need to be changed. 
  -----
    Very important
    If all i/o gets pushed through the serializer (back or front), then queued items can own state through 
      the computation and return new state or db.  we don't have to add it through various points

    - a ordinate job that completes cannot alter state, but can only queue. because the new state
      is a result of the serializer job. 
       
    - an advantage - we don't have to have partial jobs ...  that call a continuation Action f

    - we can still do partial stuff, if wanted by pushing another message on back of queue. 

    BUT - must keep queue out of state. because parallel jobs that run. will compete.
      - think this is good anyway.

    
*)

let manage_chain1 (state : Misc.my_app_state) e    =
  match e with

    (* TODO connection errors should monitor read errors and clear fd *)
    | U.GotMessage ( conn , header, raw_header, payload) -> (

      let now = Unix.time () in
      match header.command with
        | "inv" -> (
          (*	- we accept a block inventory from any peer, since if synced any peer could 
            have the latest mined block first
            - but we prioritize solicited inventory over unsolicted inventory  
            - if unsolicited then we ignore if already have unsolicited from peer
            - also block_inv_pending is used to avoid sending requests too often when synched
          *)
          (* extract blocks from inventory message, and filter for those we don't know *)
          let _, inv = M.decodeInv payload 0 in
          let block_hashes =
            inv
            |> L.filter (fun (inv_type,hash) ->
              inv_type = 2
           (*   && not (U.SS.mem hash state.heads ) *)
              && not ( U.SS.mem hash state.blocks_on_request))  (* eg. ignore if we've already requested the block *)
            |> L.map (fun (_,hash) -> hash)
          in
          (* did we ask for this inv *)
          let solicited =
              match state.block_inv_pending with
              | Some (fd, _) when fd == conn.fd -> true
              | _ -> false
          in
          (* prioritize handling *)
          let block_hashes =
              if solicited then 
                block_hashes 
              else (
                (* ignore if already have blocks on request from the same peer *)
                if U.SS.exists (fun _ (fd,_,_) -> conn.fd == fd) state.blocks_on_request then
                  []
                else
                  block_hashes (* may want to take just one *)
              )
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
              L.fold_left (fun m h -> U.SS.add h (conn.fd, now, solicited) m) state.blocks_on_request block_hashes
            in
            ( { state with
                block_inv_pending = block_inv_pending;
                blocks_on_request = blocks_on_request ;
				      jobs = state.jobs @  
              [
                log @@ U.format_addr conn
                  ^ " *** got inventory blocks " ^ (string_of_int @@ L.length block_hashes  )
                  ^ " - on request " ^ string_of_int @@ U.SS.cardinal blocks_on_request ;
                  (* request blocks from the peer *)
                  U.send_message conn (initial_getdata block_hashes );
              ]
				}
            )
          else
            state
          )

        | "block" -> (
          (* we received a block *)
          let hash = (M.strsub payload 0 80 |> M.sha256d |> M.strrev ) in
          let _, header = M.decodeBlock payload 0 in

          (* if we don't yet have the block, and it links into chain then include 
              ok, we are committed to writing the block to disk and including, 
              then lets do the io, first not screate another message
          *)
(*
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
*)
          (* update the time that we got a valid block from the peer *)
          let last =
              (* update the fd to indicate we got a good block, TODO tidy this *)
              let last = L.filter (fun (x : Misc.ggg) -> x.fd != conn .fd) state.last_block_received_time in
              ({ fd = conn.fd; t = now ;
              } : Misc.ggg )::last
          in
          (* remove from blocks on request *)
          let blocks_on_request = U.SS.remove hash state.blocks_on_request in
          let y () = 
            let x = Processblock.(  
              {
                block_count = 0;
                db = state.db;
              })
            in
              log "\nbegin writing db"
              >> Processblock.process_block x payload 
              >> log "done writing db"
          in
          { state with
            (* heads = heads; *)
            blocks_on_request = blocks_on_request;
            last_block_received_time = last;
            seq_jobs_pending = Myqueue.add state.seq_jobs_pending y ;
            jobs = state.jobs @ 
            [ 
              log @@ U.format_addr conn ^ " block " ^ M.hex_of_string hash ^ 
              " on request " ^ string_of_int @@ U.SS.cardinal blocks_on_request 
            ]
			}
        )
        | _ -> state
      )
    | _ -> state

(*
    So we need a db connecttion....
    to see what blocks we need...
*)

let manage_chain2 (state : Misc.my_app_state) e  =
  (* issue inventory requests for blocks based on current chainstate leaves *)
  match e with
    | U.Nop -> state
    | _ ->
      (* we need to check we have completed handshake *)
      (* shouldn't we always issue a request when blocks_on_request *)
      let now = Unix.time () in

      (* if peer never responded to an inv, clear the pending flag *)
      let state = { state with 
        block_inv_pending = match state.block_inv_pending with
          | Some (_, t) when now > t +. 60. -> None
          | x -> x 
        }
      in

      (* if a block was requested at least 60 seconds ago, and
      we haven't received any valid blocks from the corresponding peer for at least 60 seconds, then
      clear from blocks_on_request to permit re-request from a different peer *)
      let state = { state with
        blocks_on_request = U.SS.filter (fun hash (fd,t, solicited) ->
          not (
            now > t +. 60.
            && match CL.find state.last_block_received_time (fun x -> x.fd == fd) with
              | Some x -> now > x.t +. 60.
              | None -> true
            )
          ) state.blocks_on_request
      } in

      (* are there solicited blocks on request *)
      let has_solicited = 
        U.SS.exists (fun _ (_,_,solicited) -> solicited) state.blocks_on_request 
      in

      (* if only unsolicited blocks on request, and no inv pending then make an inv request *)
      if not has_solicited 
        && state.block_inv_pending = None 
        && state.connections <> [] 
        && state.seq_jobs_pending = Myqueue.empty
        then

(*
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
*)
        (* choose a peer fd at random *)
        let index = now |> int_of_float |> (fun x -> x mod List.length state.connections ) in
        let (conn : U.connection) = List.nth state.connections index in
        (* TODO fixme *)
        (* let head = (M.string_of_hex "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f") in *)
        (* TODO we need to record if handshake has been performed *)


        let job () = 
          log @@ S.concat "" [
            "request addr " ; conn.addr;
            "\nblocks on request " ; string_of_int (U.SS.cardinal state.blocks_on_request) ;
            (* "\nheads count " ; string_of_int (L.length heads); *)
            (* "\nrequested head is ";  M.hex_of_string head ; *)
            "\n fds\n" ; S.concat "\n" ( L.map (fun (x : Misc.ggg) -> string_of_float (now -. x.t ) ) state.last_block_received_time )
            ]
            >> Misc.PG.begin_work state.db

            >> Misc.PG.prepare state.db  ~query:"select pb from leaves order by random() limit 1" ()
            >> Misc.PG.execute state.db  ~params:[ ] ()
            >>= fun rows -> 
              let head = 
                match rows with
                  (Some field ::_ )::_ -> Misc.PG.bytea_of_string field
                  | _ -> raise (Failure "couldn't get leaf")
            in 
            Misc.PG.commit state.db
            >> log @@ "\nrequested head is " ^ M.hex_of_string head
            >>
            (* request inv 
                TODO What if the conn has been closed in the meantime???k
                we should post a message with the tip... to use a known good connection
            *)
            U.send_message conn (initial_getblocks head)
          in 
          { state with
          block_inv_pending = Some (conn.fd, now ) ;
          seq_jobs_pending = Myqueue.add state.seq_jobs_pending job ;
		  (* jobs = state.jobs @ [ y (); ] *)
		      }
      else
        state

(*
    so a lot of the job will 
    - if we bind the fd to use, then it could get closed by something else...
*)



(*
    this whole function should go. 
    we use db state to determine what blocks we have or need.
*)
(*
let readBlocks fd =
  let advance fd len =
      Lwt_unix.lseek fd len SEEK_CUR 
      >>= fun r -> 
      (* Lwt_io.write_line Lwt_io.stdout @@ "seek result " ^ string_of_int r  *)
      return ()
  in
  (* to scan the messages stored in file *)
  let rec loop fd ( heads : U.my_head U.SS.t ) =
    U.read_bytes fd 24
    >>= fun x -> match x with 
      | Some s -> ( 
        let _, header = M.decodeHeader s 0 in
        (* Lwt_io.write_line Lwt_io.stdout @@ header.command ^ " " ^ string_of_int header.length >> *) 
        U.read_bytes fd 80 
        >>= fun u -> match u with 
          | Some ss -> 
            let hash = ss |> Message.sha256d |> Message.strrev  in
            let _, block_header = M.decodeBlock ss 0 in

            (* Core.Core_map.find *)
			(* should be refactored to function - get_height and called with args *)
            let height = 
              if (U.SS.mem block_header.previous heads) then 
                (U.SS.find block_header.previous heads ).height + 1 
              else
                1     (* we have a bug, where we never download the first block *)
            in

            (* Lwt_io.write_line Lwt_io.stdout @@ M.hex_of_string hash ^ " " ^ M.hex_of_string block_header.previous 
            >> *) advance fd (header.length - 80 )
            >> let heads = U.SS.add hash ({ 
                previous = block_header.previous;  
                height = height; 
              } : U.my_head ) heads 
            in
            loop fd  heads  (* *)
          | None -> 
            return heads 
        )
      | None -> 
        return heads 
  in    
    loop fd U.SS.empty  
    >>= fun heads -> return heads
*) 


let create () =
  (* initialization should be an io function? 
    
    how can we initialize if the fd failed????

    we could use continuation passing style, so if the action failed, 
      we could output the error
      we kind of also need to catch file exceptions.
   *)
  U.write_stdout "**** initializing chain "
 

 
  (* open blocks.dat writer *)
  >> Lwt_unix.openfile "blocks.dat"  [O_RDWR ; O_CREAT ] 0o644 
  (*>> Lwt_unix.openfile "blocks.dat__"  [O_RDONLY (*; O_APPEND*) ; O_CREAT ] 0o644  *)

(*
  >>= fun blocks_fd -> 
      match Lwt_unix.state blocks_fd with
        | Opened -> ( 
          readBlocks blocks_fd  
          >>= fun heads ->
            U.write_stdout ( "**** heads size " ^ (string_of_int (U.SS.cardinal heads) ))
          >>
          let heads =
            if U.SS.cardinal heads = 0 then  
              U.SS.empty
              |> U.SS.add (M.string_of_hex "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f") 
              (* |> U.SS.add ( M.zeros 32) *) (* for some reason nodes respond with second block,... *)
               ({
                previous = "";
                height = 0;
              } : U.my_head )
            else 
              heads
          in 

         let ret =   heads , blocks_fd 
         in
        return (Some  ret )
        ) 
      | _ -> return None
*)
(* there's an issue that jobs are running immediately before placing in choose() ?  *)




let update state e =
  let state = manage_chain1 state e in
  let state = manage_chain2 state e in
  state



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

