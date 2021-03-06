
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

(*
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
*)

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

