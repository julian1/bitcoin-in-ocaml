  


  let add_jobs jobs state = { state with jobs = jobs @ state.jobs } 
let get_another_block peer state =

    if List.length peer.blocks_inv > 0 
    && List.length peer.blocks_pending = 0   then

      let encodeInventory jobs =
        let encodeInvItem hash = encodeInteger32   2 ^ encodeHash32 hash in 
          (* encodeInv - move to Message  - and need to zip *)
          encodeVarInt (List.length jobs )
          ^ String.concat "" @@ List.map encodeInvItem jobs 
      in
      let now = Unix.time () in
      let index = now |> int_of_float |> (fun x -> x mod List.length peer.blocks_inv) in 
      let hash = List.nth peer.blocks_inv index in
      let payload = encodeInventory [ hash ] in 
      let header = encodeHeader {
        magic = m ;
        command = "getdata";
        length = strlen payload;
        checksum = checksum payload;
      }
      in
      state

      |> remove_peer peer 
      |> fun state -> 
        let peer = { peer with 
          blocks_inv = List.filter (fun x -> x != hash ) peer.blocks_inv;
          blocks_pending = hash :: peer.blocks_pending;
      }  in add_peer peer state 

(*    -- are there two read messages ??? *)

      |> add_jobs [ 
        log @@ 
          "requesting block " ^ (hex_of_string hash ) ^ " from " ^ peer.conn.addr 
          ^ " inv count " ^ (string_of_int (List.length peer.blocks_inv ) ) 
          ^ " pend count " ^ (string_of_int (List.length peer.blocks_pending ) ) 
        >> send_message peer.conn (header ^ payload);  

Lwt_unix.lseek fd pos Unix.SEEK_SET >>= fun pos' ->
assert (pos' = pos);
let block = String.create bs in
_read_buf fd block 0 bs >>= fun () ->
Lwt.return block

      (* let (ic : 'mode Lwt_io.channel )= Lwt_io.of_fd ~mode:Lwt_io.input fd in *)

    let read_bytes fd len =
      let block = Bytes .create len in
      Lwt_unix.read fd block 0 len >>= 
      fun ret ->
        (* Lwt_io.write_line Lwt_io.stdout @@ "read bytes - "  ^ string_of_int ret >>  *)
      return (
        if ret > 0 then Some ( Bytes.to_string block )
        else None 
        )
    in
    let advance fd len =
        Lwt_unix.lseek fd len SEEK_CUR 
        >>= fun r -> 
        (* Lwt_io.write_line Lwt_io.stdout @@ "seek result " ^ string_of_int r  *)
        return ()
    in
    (* to scan the messages stored in file *)
    let rec loop fd heads =
      read_bytes fd 24
      >>= fun x -> match x with 
        | Some s -> ( 
          let _, header = decodeHeader s 0 in
          (* Lwt_io.write_line Lwt_io.stdout @@ header.command ^ " " ^ string_of_int header.length >> *) 
          read_bytes fd 80 
          >>= fun u -> match u with 
            | Some ss -> 
              let hash = ss |> Message.sha256d |> Message.strrev  in
              let _, block_header = decodeBlock ss 0 in
              (* Lwt_io.write_line Lwt_io.stdout @@ 
                hex_of_string hash 
                ^ " " ^ hex_of_string block_header.previous 
              >> *) advance fd (header.length - 80 )
              >> let heads = SS.add hash { 
                  previous = block_header.previous;  
                  height = 0; 
                } heads 
              in
              loop fd  heads  (* *)
            | None -> 
              return heads 
          )
        | None -> 
          return heads 
    in 

    (* this bloody thing throws - if it can't open *)

    Lwt_unix.openfile "blocks.dat"  [O_RDONLY] 0 
    >>= fun fd -> 
      let u =  (* very strange that this var is needed to typecheck *)
      match Lwt_unix.state fd with 
        Opened -> 
          Lwt_io.write_line Lwt_io.stdout "scanning blocks..." 
          >> loop fd SS.empty  
          >>= fun heads -> Lwt_unix.close fd
          >> return heads
        | _ -> return 
            ( SS.empty 
            |>
            let genesis = string_of_hex "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f" in 
            SS.add genesis 
           { 
            previous = ""; 
            height = 0; 
          })
        in u
*)
   >>= fun heads ->   
      Lwt_io.write_line Lwt_io.stdout @@ "blocks read " ^ string_of_int (SS.cardinal heads  )

    >>
    (* get initial state up 
      uggh opening the file, non truncate ... 
    *)
(*     Lwt_io.open_file ~flags: [O_WRONLY ] Lwt_io.output       "blocks.dat"  *)  



    Lwt_unix.openfile "blocks.dat"  [O_WRONLY ; O_APPEND ; O_CREAT ] 0  
    >>= fun fd -> ( 
      match Lwt_unix.state fd with
        | Opened -> let blocks_oc = Lwt_io.of_fd ~mode:Lwt_io.output fd  in return ()
        | _ -> return ()
    )
    >>
(*
Lwt_unix.lseek fd pos Unix.SEEK_SET >>= fun pos' ->
assert (pos' = pos);
let block = String.create bs in
_read_buf fd block 0 bs >>= fun () ->
Lwt.return block

      (* let (ic : 'mode Lwt_io.channel )= Lwt_io.of_fd ~mode:Lwt_io.input fd in *)
*)

(*
    let read_bytes fd len =
      let block = Bytes .create len in
      Lwt_unix.read fd block 0 len >>= 
      fun ret ->
        (* Lwt_io.write_line Lwt_io.stdout @@ "read bytes - "  ^ string_of_int ret >>  *)
      return (
        if ret > 0 then Some ( Bytes.to_string block )
        else None 
        )
    in
    let advance fd len =
        Lwt_unix.lseek fd len SEEK_CUR 
        >>= fun r -> 
        (* Lwt_io.write_line Lwt_io.stdout @@ "seek result " ^ string_of_int r  *)
        return ()
    in
    (* to scan the messages stored in file *)
    let rec loop fd heads =
      read_bytes fd 24
      >>= fun x -> match x with 
        | Some s -> ( 
          let _, header = decodeHeader s 0 in
          (* Lwt_io.write_line Lwt_io.stdout @@ header.command ^ " " ^ string_of_int header.length >> *) 
          read_bytes fd 80 
          >>= fun u -> match u with 
            | Some ss -> 
              let hash = ss |> Message.sha256d |> Message.strrev  in
              let _, block_header = decodeBlock ss 0 in
              (* Lwt_io.write_line Lwt_io.stdout @@ 
                hex_of_string hash 
                ^ " " ^ hex_of_string block_header.previous 
              >> *) advance fd (header.length - 80 )
              >> let heads = SS.add hash { 
                  previous = block_header.previous;  
                  height = 0; 
                } heads 
              in
              loop fd  heads  (* *)
            | None -> 
              return heads 
          )
        | None -> 
          return heads 
    in 

    (* this bloody thing throws - if it can't open *)

    Lwt_unix.openfile "blocks.dat"  [O_RDONLY] 0 
    >>= fun fd -> 
      let u =  (* very strange that this var is needed to typecheck *)
      match Lwt_unix.state fd with 
        Opened -> 
          Lwt_io.write_line Lwt_io.stdout "scanning blocks..." 
          >> loop fd SS.empty  
          >>= fun heads -> Lwt_unix.close fd
          >> return heads
        | _ -> return 
            ( SS.empty 
            |>
            let genesis = string_of_hex "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f" in 
            SS.add genesis 
           { 
            previous = ""; 
            height = 0; 
          })
        in u
*)
   >>= fun heads ->   
      Lwt_io.write_line Lwt_io.stdout @@ "blocks read " ^ string_of_int (SS.cardinal heads  )

    >>
    (* get initial state up 
      uggh opening the file, non truncate ... 
    *)
(*     Lwt_io.open_file ~flags: [O_WRONLY ] Lwt_io.output       "blocks.dat"  *)  



    Lwt_unix.openfile "blocks.dat"  [O_WRONLY ; O_APPEND ; O_CREAT ] 0  
    >>= fun fd -> ( 
      match Lwt_unix.state fd with
        | Opened -> let blocks_oc = Lwt_io.of_fd ~mode:Lwt_io.output fd  in return ()
        | _ -> return ()
    )
    >>

        let peer = { conn = conn; blocks_inv = [] ; blocks_pending  = [] ;   blocks_inv_last_request_time = 0. } in
        state
        |> add_peer  peer
        |> add_jobs [
          log @@ "whoot got connection " ^ format_addr conn   ^
            "\npeers now " ^ ( string_of_int @@ List.length state.peers)
          (* or separate? *) 
          >> send_message peer.conn initial_version 
          >> log @@ "*** sent our version " ^ format_addr conn
          ;
           get_message peer.conn
        ] 
      
      | GotConnectionError msg ->
        add_jobs [ 
          log @@ "could not connect " ^ msg >> return Nop
          ] state 

      | GotMessageError (conn, msg) ->

        let peer = peer_of_conn conn state in

        let fd = conn.fd in
        state 
        |> remove_peer peer
        |> fun state -> 
          add_jobs [ 
          log @@ "could not read message " ^ msg
          ^ "\npeers now " ^ ( string_of_int @@ List.length state.peers)
          ;

          match Lwt_unix.state fd with
            Opened -> ( Lwt_unix.close fd ) >> return Nop 
            | _ -> return Nop 
          (*in
          log @@ "closing descriptor - state " ^ s
          >> log "done closing descriptor " 
          *)
        ] state

      | GotMessage (conn, header, raw_header, payload) -> 
        match header.command with
          | "version" ->
            (* let peer = peer_of_conn conn state in  *)
            state
            |> add_jobs [ 
              (* should be 3 separate jobs? *)
              log "got version message"
              >> send_message conn initial_verack
              >> log @@ "*** whoot wrote verack " ^ format_addr conn;
              get_message conn 
            ] 

          | "verack" ->
            add_jobs [ 
              (* should be 3 separate jobs? *)
              log "got verack - requesting addr"
              >> send_message conn initial_getaddr ;
              get_message conn 
            ] state 


(*
    - we don't care about a block unless it can advance a head ?   
    except it might be a fork arising from a point further back in the chain.
    ----
    tests, for performing block inv request
    1. we haven't already got it.
  
    tests for inclusion
    2. it advances on another block we know about (from anywhere in sequence - not just the tip, )  

  - ok so we have a bunch of nodes telling us about blocks and we are forced to try and download them...
    the way to avoid is to have all blocks...

  - EXTREMELY IMPORTANT we need to encode older block-hashes in our getdata request. so that a node on a fork
    can return it's latest if we 

  - as a first pass lets just save the blockchain as we go, and see how fast/slow it is on startup
    we can optimize later.
*)

(*
    - so peer gives us inventory...
    we should store it. and that's about it.   
    we can request one of the blocks at randome as well...

    OK, the peer count should be coming down as we pull off the blocks
    and download them...
*)
                  (* VERY IMPORTANT - when we bind in the peer here, it means that we don't
                  get the changes to the peer made later
                  - to make it atomic we can only do the add_jobs once - right at the end.
                  - and the state peers are a copy of the real peers bound into jobs - which 
                    is terrible.

                  - the peer stuff should only be in one place. we can compute stats for the
                  app state if we want..  

                  - getting rid of this ought to simplify...

                  - alternatively we only bind the file descriptor into the read and keep everything 
                  else in the main state. (so it's only recorded once ). means there's a lookup. 
                  - eg. there might be several events on the peer...

                  - we're also binding far too much information into the message handlers
                  that don't really need it. 

                  - does this also mean we can flatten out the peer and conn stuff ?
  
                  - ok, as a first pass lets just move the peer information into state
                  *)    
          | "inv" ->
            state
            |> fun state -> 
              let now = Unix.time () in
              let _, inv = decodeInv payload 0 in
              (* add inventory blocks to list in peer *)
              let needed_inv_type = 2 in
              let block_hashes = inv
                |> List.filter (fun (inv_type,_) -> inv_type = needed_inv_type )
                |> List.map (fun (_,hash)->hash)
                (* ignore blocks we already have *)
                |> List.filter (fun hash -> not @@ SS.mem hash state.heads )
              in

              let peer = peer_of_conn conn state in  
              let peer = { peer with blocks_inv = block_hashes @ peer.blocks_inv  } in
              update_peer peer state

              |> add_jobs [ 
                  log @@ "\ngot inv " ^ conn.addr 
                    ^ " inv count " ^ (string_of_int (List.length peer.blocks_inv ) ) 
                    ^ " pend count " ^ (string_of_int (List.length peer.blocks_pending ) ) ; 
                  get_message conn; 
                ] 

            (* code to keep the inventory full for the peer 
        change this code to return a tuple of the new heads and new jobs
      *)
            |> fun state -> 
              (* this condition is right. if less than 100, we always do it, on timer *)
              if List.length peer.blocks_inv < 100 
                && now -. peer.blocks_inv_last_request_time > 60.
                then  
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
                add_jobs [
                  log @@ "**** requesting blocks inv " 
                  ^ " from " ^ format_addr peer.conn   
                  ^ " head count " ^ (string_of_int @@ List.length heads )
                  ^ " head " ^ hex_of_string head 
                  >> send_message conn (initial_getblocks head)
                ] state
                |> fun state -> let peer = { peer with blocks_inv_last_request_time = now } in
                update_peer peer state
              
              else
                state

            |> fun state ->  
              (* maybe download a block *) 
              get_another_block peer state 


(*
    OK, now we have a single download head that we make requests too...
    we can compute the download heads... in case of fork.
*)

(*
  VERY IMPORTNAT
  ok, i think this might be done a lot easier by just shadowing everything
  peer, heads, jobs . rather than our piping stuff
  then creating 
  - the only thin is where we factor code off into another function and 
  will have to return these shad
  - and this is what we do with if/else as well - rather than return state
  we just return what we modified. 
  - limits the scope of the vars, and makes it clear whats being done. 
*)
          | "block" ->
            (* let _, block = decodeBlock payload 0 in *)
            let hash = (Message.strsub payload 0 80 |> Message.sha256d |> Message.strrev ) in
            let _, header = decodeBlock payload 0 in 
            (* if the header hash points at a head - update our head 
              TODO - we are updating the head, before we update the db,
              when it should be the otherway around. although if db fails
              we probably have other issues

              ok, rather than maintaining the download heads... lets compute 
              when we need .
              ----
                our optimiser isn't working very well. everytime we get a random block
                it gets updated. 

                it would be really nice to see how this performs at the start when blocks are small...
            *)

            let peer = peer_of_conn conn state in  
            let peer = { peer with blocks_pending = List.filter (fun pend -> pend <> hash) peer.blocks_pending } in 
            update_peer peer state
              
            
            |> fun state -> 

              if not (SS.mem hash state.heads ) then 

                (*let heads =
                  SS.add hash { 
                    previous = header.previous;  
                    height = 0; (* head.height + 1;  *)
                  } state.heads
                in

                { state with heads = heads;  } 
                             
                |> fun state -> *) (*get_another_block peer  state

                |> fun state -> *) 
                add_jobs [ 
                  log @@ "got block  " ^ conn.addr ^ " " ^  hex_of_string hash; 
                  (*   Lwt_io.write state.blocks_oc (raw_header ^ payload ) >> return Nop ; *)
                  get_message conn ; 
                ] state 
            else
              add_jobs [ 
                log @@ "already have block ignore - " ^ hex_of_string hash;
                get_message conn ; 
              ] state 


          | "tx" ->
            (* at the moment we dont care about tx *)
            (* let _, tx = decodeTx payload 0 in *)
            add_jobs [ 
              (let hash = (payload |> Message.sha256d |> Message.strrev ) in 
              log @@ "got tx!!! "  ^ hex_of_string hash )   ; 
              get_message conn ; 
            ] state

(*
  we can manage saving connections, the same as blocks - serializing into connections.dat
  - always connect if unknown
  - if verack then add to list of known peers and record 
  - if we have less than the number of active conns we want then choose and connect 
*)
(*
  - closing app ought to be easy - we don't need cancel on individual tasks.
  - just wait till we get to choose (can have 1 sec timeout job), 
  - then create a job to close all connections, and descriptors, using details in conn - join 
  - could even have this outside the main loop. 
*)
(*
    blocks - 
      should only make inv requests for heads that trace to the main chain, not
        disconnected sequences
      but do request blocks from segments

      - this will keep it focused downloading around tip.
    -------
      -No inventory requests are relatively free.
      -Do one every ten seconds on the main node (or all heads that trace to root). 
      - then filter against blocks we have
      - and filter against pending (with a timeout )
      - and record new list as blocks that are pending and can be accepted
      - then distribute block requests to different conns

      - if seomthing is pending, we don't want to re-request it.
      but, if something stalls we do.
        - so we just record the time it was requested. 

 *)         | "addr" -> 
              let pos, count = decodeVarInt payload 0 in
              (* should take more than the first *)
              let pos, _ = decodeInteger32 payload pos in (* timeStamp  *)
              let _, addr = decodeAddress payload pos in 
              let formatAddress (h : ip_address ) =
                let soi = string_of_int in
                let a,b,c,d = h.address  in
                String.concat "." [
                soi a; soi b; soi c; soi d 
                ] (* ^ ":" ^ soi h.port *)
              in
              let a = formatAddress addr in
              (* ignore, different instances on different ports *)
              let already_got = List.exists (fun peer -> peer.conn.addr = a (* && peer.conn.port = addr.port *) ) 
                  state.peers 
              in
              if already_got || List.length state.peers >= 30 then  
                add_jobs [ 
                  log @@ "whoot new addr - already got or ignore " ^ a  
                  ;
                  get_message conn 
                ] state 
              else 
                add_jobs [ 
                 log @@ "whoot new unknown addr - count "  ^ (string_of_int count ) 
                  ^  " " ^ a ^ " port " ^ string_of_int addr.port ;  
                  get_connection (formatAddress addr) addr.port ;
                  get_message conn 
              ] state

          | s ->
            add_jobs [ 
              log @@ "message " ^ s ;
              get_message conn 
            ] state

  in new_state 


