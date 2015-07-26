
module M = Message
module U = Util
module S = String

module L = List
module CL = Core.Core_list



let (>>=) = Lwt.(>>=) 
let return = Lwt.return

(*
  VERY VERY IMPORTANT

    - rather than requesting the tip hash, we could request tip - 1. 
    - then when we get a block inventory back, we will get the tip hash back
      and know it's what we want.
       
    - we might be able to remove the solicited stuff.
    - we still need to handle, random single blocks that advance the chain.
*)

(*
  - ok, now we need more block rules (merckle root, difficulty, time checks )
  - then we need to save...

    RIGHT - the jobs are actually jobs in an array... 
            are we sure we don't want 
*)


(* TODO change name encode_getblocks_message *)
let initial_getblocks network starting_hash =
  (* the list are the options, and peer will return a sequence
    from the first valid block in our list *)
  (* TODO should be list of hashes *)
  let payload =
    M.encodeInteger32 1  (* version *)
    ^ M.encodeVarInt 1
    ^ M.encodeHash32 starting_hash
    ^ M.zeros 32   (* block to stop - we don't know should be 32 bytes *)
  in
  M.encodeMessage network "getblocks" payload


(* TODO change name encode_getdata_message *)
let initial_getdata network hashes =
  (* 2 means block hashes only *)
  let encodeInventory hashes =
    let encodeInvItem hash = M.encodeInteger32 2 ^ M.encodeHash32 hash in
      (* encodeInv - move to Message  - and need to zip *)
      M.encodeVarInt (L.length hashes )
      ^ S.concat "" @@ L.map encodeInvItem hashes
  in let hashes = match network with 
      | M.Bitcoin -> hashes 
      | M.Litecoin -> L.rev hashes 
  in
  let payload = encodeInventory hashes in
  M.encodeMessage network "getdata" payload


let log s = U.write_stdout s >> return U.Nop

(*
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

(*  - as well as fold_m should have takeWhile ...
*)
let fold_m f acc lst =
  let adapt f acc e = acc >>= fun acc -> f acc e in
  L.fold_left (adapt f) (return acc) lst



let manage_chain1 (state : U.my_app_state) e    =
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
          (
          if block_hashes <> [] then ( 
            log @@ "\n@@@ inv blocks - checking db for blocks " ^ string_of_int (L.length block_hashes)
            >> U.PG.begin_work state.db
            >> U.PG.prepare state.db ~query:"select exists ( select * from block where hash = $1 )" ()
            >> let f x hash = 
              U.PG.execute state.db ~params:[ Some (U.PG.string_of_bytea hash) ] ()
              >>= function 
                (Some "f"::_ )::_ -> return (hash :: x) 
                | _ -> return x
              in 
              fold_m f [] block_hashes 
            >>= fun block_hashes ->
              U.PG.commit state.db
            >> log @@ "@@@ done checking db for blocks " ^ string_of_int (L.length block_hashes)
            >> return block_hashes 
            )
          else
            return []
          )
          >>= fun block_hashes -> 

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
            let state = { state with
                block_inv_pending = block_inv_pending;
                blocks_on_request = blocks_on_request ;
				    } in
            let jobs =  
              [
                log @@ U.format_addr conn
                  ^ " *** got inventory blocks " ^ (string_of_int @@ L.length block_hashes  )
                  ^ " - on request " ^ string_of_int @@ U.SS.cardinal blocks_on_request ;
                  (* request blocks from the peer *)
                  U.send_message conn (initial_getdata state.network block_hashes );
              ]
            in
            return (U.SeqJobFinished (state, jobs))

          else
            return (U.SeqJobFinished (state, []))
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
              let last = L.filter (fun (x : U.ggg) -> x.fd != conn .fd) state.last_block_received_time in
              ({ fd = conn.fd; t = now ;
              } : U.ggg )::last
          in
          (* remove from blocks on request *)
          let blocks_on_request = U.SS.remove hash state.blocks_on_request in

          let x = Processblock.(  
            {
              block_count = 0;
              db = state.db;
            })
          in

          (* OK. now we have to run this computation inline *) 
          log "\nbegin writing db"
          >> Processblock.process_block x payload 
          >> log "done writing db"
          >>

          let state = { state with
            (* heads = heads; *)
            blocks_on_request = blocks_on_request;
            last_block_received_time = last;
            (* seq_jobs_pending = Myqueue.add state.seq_jobs_pending y ; *) 
			    } in
          let jobs = [ 
              log @@ U.format_addr conn ^ " block " ^ M.hex_of_string hash ^ 
              " on request " ^ string_of_int @@ U.SS.cardinal blocks_on_request 
          ] 
          in
          return (U.SeqJobFinished (state, jobs ))
        )
        | _ -> return (U.SeqJobFinished (state, []))
      )
    | _ -> return (U.SeqJobFinished (state, []))

(*
    So we need a db connecttion....
    to see what blocks we need...
*)

let manage_chain2 (state : U.my_app_state) e  =
  (* issue inventory requests for blocks based on current chainstate leaves *)
  match e with
    | _ ->
      (* we need to check we have completed handshake *)
      (* shouldn't we always issue a request when blocks_on_request *)
      let now = Unix.time () in

      (* if peer never responded to an inv, clear the pending flag *)
      let state = { state with 
        block_inv_pending = match state.block_inv_pending with
          | Some (_, t) when now > t +. 15. -> None
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
        (* && state.seq_jobs_pending = Myqueue.empty *)
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

        log @@ S.concat "" [
          "request addr " ; conn.addr;
          "\nblocks on request " ; string_of_int (U.SS.cardinal state.blocks_on_request) ;
          (* "\nheads count " ; string_of_int (L.length heads); *)
          (* "\nrequested head is ";  M.hex_of_string head ; *)
          "\n fds\n" ; S.concat "\n" ( L.map (fun (x : U.ggg) -> string_of_float (now -. x.t ) ) state.last_block_received_time )
          ]
        >> U.PG.begin_work state.db
        (* >> U.PG.prepare state.db ~query:"select hash from _leaves order by random() limit 1" () *)
        >> U.PG.prepare state.db ~query:"select hash from _leaves2 order by height" ()
        >> U.PG.execute state.db ~params:[ ] ()
        >>= fun rows -> 
          let hashes = L.map (function (Some field ::_ ) -> U.PG.bytea_of_string field ) rows in
          let head = L.hd hashes in
(*
          let head = 
            match rows with
              (Some field ::_ )::_ -> U.PG.bytea_of_string field
              | _ -> raise (Failure "couldn't get leaf")
        in 
*)
        U.PG.commit state.db
        >> log @@ "\nrequested head is " ^ M.hex_of_string head
        >> 
        let state = { state with
            block_inv_pending = Some (conn.fd, now ) ;
        } in
        let jobs = [
          U.send_message conn (initial_getblocks state.network head)
        ] in
        return (U.SeqJobFinished (state, jobs))

      else
        return (U.SeqJobFinished (state, []))

(* 
  VERY IMPORTANT 
  the queue doesn't go deep. 

  - Because we don't try to read the next readMessage until the GotMessage has processed...
  - Perhaps we should change - so that we rebind the read handler as soon as we read something. 
  - this would be nice, in that we wouldn't need to always rebind the handler in the p2p loop ..
*)

let update state e = 
  manage_chain1 state e 
  >>= fun (U.SeqJobFinished (state, jobs1)) ->
    manage_chain2 state e
  >>= fun (U.SeqJobFinished (state, jobs2)) ->
    return (U.SeqJobFinished (state, jobs1 @ jobs2))
 


(*
  ok, we can't chain things together because we return state and jobs .... 

  val update : Util.my_app_state -> Util.my_event -> (Util.my_event ) Lwt.t
*)

let update_ state e =
  let state = manage_chain1 state e in
(*  let state = manage_chain2 state e in *)
  state




