(*
corebuild    -package leveldb,microecc,cryptokit,zarith,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax  src/client3.byte

*)
open Message
open Script

open Lwt (* for >>= *)


(*  bitcoin magic_head: "\xF9\xBE\xB4\xD9",
  testnet magic_head: "\xFA\xBF\xB5\xDA",
  litecoin magic_head: "\xfb\xc0\xb6\xdb",
*)

let m = 0xd9b4bef9  (* bitcoin *)
(* let m = 0xdbb6c0fb   litecoin *)

(* initial version message to send *)
let initial_version =
  let payload = encodeVersion {
      protocol = 70002;
      nlocalServices = 1L; (* doesn't seem to like non- full network 0L *)
      nTime = 1424343054L;
      from = { address = 127,0,0,1; port = 8333 };
      to_ = { address = 50,68,44,128; port = 8333 };
      (* nonce = -4035119509127777989L ; *)
      nonce = -8358291686216098076L ;
      agent = "/Satoshi:0.9.3/"; (* "/bitcoin-ruby:0.0.6/"; *)
      height = 127953;
      relay = 0xff;
  } in
  let header = encodeHeader {
    magic = m ;
    command = "version";
    length = strlen payload;
    checksum = checksum payload;
  } in
  header ^ payload


(* verack response to send *)
let initial_verack =
  let header = encodeHeader {
    magic = m ;
    command = "verack";
    length = 0;
    (* clients seem to use e2e0f65d - hash of first part of header? *)
    checksum = 0;
  } in
  header


let initial_getaddr =
  let header = encodeHeader {
    magic = m ;
    command = "getaddr";
    length = 0;
    (* clients seem to use e2e0f65d - hash of first part of header? *)
    checksum = 0;
  } in
  header


let initial_getblocks starting_hash =
  (* we can only request one at a time 
    - the list are the options, and server returns a sequence
    from the first valid block in our list
  *)
  let payload =
    encodeInteger32 1  (* version *)
    ^ encodeVarInt 1
    ^ encodeHash32 starting_hash

 (*   ^ encodeHash32 (string_of_hex "00000000839a8e6886ab5951d76f411475428afc90947ee320161bbf18eb6048"  ) *)
(*    ^ encodeHash32 ( string_of_hex "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"  ) *)
                        (* this isn't right it should be a hash *)
    ^ zeros 32   (* block to stop - we don't know should be 32 bytes *)
	in
  let header = encodeHeader {
    magic = m ;
    command = "getblocks";
    length = strlen payload;
    checksum = checksum payload;
  } in
	header ^ payload



type connection =
{
  (* addr : ip_address;
    when checking if connecting to same node, should check ip not dns name
    *) 
  addr : string ; 
  port : int;
  fd :  Lwt_unix.file_descr ;
  ic : Lwt_io.input Lwt_io.channel ; 
  oc : Lwt_io.output Lwt_io.channel ; 

}


type peer =
{
	conn : connection ; 

	block_inv : string list ;  (* inv items to work through *) 

	block_pending : bool ;
}



type my_action =
   | GotConnection of connection
   | GotConnectionError of string
   | GotMessage of peer * header * string * string
   | GotMessageError of peer * string (* change name MessageError - because *)
   | Nop




(* we can aggregate actions together

  as much as we want, up until the point that
  we want to manipulate the main state.

  eg.
    resolve -> connect -> send -> version -> receive ack

  can be done in one sequence, after which we probably
  want to add the connections and log stuff

  Except - with a choose - it will have to restart everything

  no, they will be sub threads
*)




let get_connection host port =
  (* what is this lwt entry *)
  Lwt_unix.gethostbyname host  (* FIXME this should be in lwt catch as well *)
  >>= fun entry ->
    if Array.length entry.Unix.h_addr_list = 0 then
      return @@ GotConnectionError "could not resolve hostname"
    else
      let a_ = entry.Unix.h_addr_list.(0) in
      let a = Unix.ADDR_INET ( a_ , port) in 
      let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let (inchan : 'mode Lwt_io.channel )= Lwt_io.of_fd ~mode:Lwt_io.input fd in
      let outchan = Lwt_io.of_fd ~mode:Lwt_io.output fd 
      in
      Lwt.catch
        (fun  () ->
          Lwt_unix.connect fd a
          >> 
          let conn = {  
              addr = Unix.string_of_inet_addr a_;
              port = port;
              fd = fd;
              ic = inchan ;
              oc = outchan; 
          } in
          return @@ GotConnection conn 
        )
        (fun exn ->
          (* must close *)
          let s = Printexc.to_string exn in
		      Lwt_unix.close fd 
          >> return @@ GotConnectionError s
        )

(*
  TODO
 
  - we need a ping/pong response, to ensure we stay connected

    - if we've saturated max connections, and a connection hasn't sent anything
    for a period (eg. 2 minutes) then drop one connection. this ought to 
    enable us to cycle to always active not just open connections.

    - done - let read top level loop handle closing of sockets. this enables comparison
    on fd first to remove the connection. instead of addr port.
      (actually not really needed now using physical equality comparison) 

    - done - guard payload length to ocaml max string length - no it's huge on 64bit. 

    - change the order of add_job so we can pipe it...
        actually we should pass an array for multiple jobs. 
*)

(* read exactly n bytes from channel, returning a string
  - change name to readn or something? *)

(* timeout throws an exception which is horrible 
  - althought we could just wrap it inside the pick to emit a value ...
*)

let readChannel inchan length (* timeout here *)  =
  let buf = Bytes.create length in

  Lwt.pick [
    Lwt_unix.timeout 180. 
    (* >> Lwt_io.write_line Lwt_io.stdout "timeout!!!" doesn't run *)
    ;
    Lwt_io.read_into_exactly inchan buf 0 length
  ] 
  >>= fun _ ->
    return @@ Bytes.to_string buf

(*
  hmmnn we want to use this thing again... when reading from file ...
  without the network timeout...
*)

let get_message peer =
    Lwt.catch (
      fun () -> 
      (* read header *)

        let ic = peer.conn.ic in 
        readChannel ic 24
        >>= fun s ->
          let _, header = decodeHeader s 0 in
          if header.length < 10*1000000 then
            (* read payload *)
            readChannel ic header.length
            >>= fun p -> 
            return @@ GotMessage ( peer, header, s, p)
          else
            return @@ GotMessageError (peer, "payload too big" ) 
      ) 
      ( fun exn ->  
          let s = Printexc.to_string exn in
          return @@ GotMessageError (peer, "here2 " ^ s) 
      )
       

let send_message peer s = 
    let oc = peer.conn.oc in
    Lwt_io.write oc s >> return Nop  (* message sent *)


(*


          (* do we have to close the channels as well as the descriptor?? *)
          Lwt_unix.close conn.fd 
          >>

let filterTerminated jobs =
  (* ok, hang on this might miss
    because several finish - but only one gets returned

    we need to use choosen instead.
  *)
  let f t =
   match Lwt.state t with
     | Return _ -> false
     | Fail _ -> false
     | _ -> true
  in
  List.filter f jobs
*)


(* we can also easily put a heartbeat timer *)

(*
  ok, it's not so easy...

  because the map is not going to accumulate
  changes if there are more than one...

  if we return the continuation then map
  can be used...

  VERY IMPORTANT we can use nchoose_split and get rid of the scanning...
*)
  (*
          - ok need
            - check we're not already connected - maintain a list in state
            - track the inbound...
            - protection against end of files. put in a banned list.
            - combine the fd,ic,oc address into a structure...
            - close conns...

        *)
                (* we sure it's not a version? 
                  ok, we want to look a bit closer...
          -- we actually want to read the raw bytes...

          1 114 58 13 85 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 255 255 31 186 250 186 32 141
                *)




(* 
	if we use this - then change name to expected 

type pending_request =
{
    addr1 : string  ; (* or conn? *)
}
*)

(*
	heads can be computed by scanning the indexes at startup.
	there's no point maintaining fast lookup memory map and leveldb btree indexes.
*)


module SS = Map.Make(struct type t = string let compare = compare end)

module SSS = Set.Make(String);; 

type my_head = 
{
  (*  hash : string; *)
    previous : string;  (* could be a list pointer at my_head *) 
    height : int;   (* if known? *)
   (*  difficulty : int ; *) (* aggregated *)
    (* bool requested *)
    (* bool have *)
}

(*
	- the blockchain (unlike connections) has to be an on-disk structure.

	- we need to be able to serialize this head structure

	- VERY IMPORTANT and an api (Lwt based) around the db, to manipulate
	the on-disk data structures.
	- eg. should wrap the db.
*)

type my_app_state =
{
(*  count : int; *)
(* pending : pending_request SS.t; *)	
(* heads : my_head SS.t;	*)

  jobs :  my_action Lwt.t list ;

  peers : peer list ; 

	(* do we do a mapping from connection to connections ? *)


	heads : my_head SS.t ;	

  (* we don't even need this - just use a single request to a random node for a random head
  every 10 seconds or so - doesn't need to be specified here 

    may only be used for fast downloading...
  *)
  time_of_last_valid_block : float; (* change name _time, or condition  *)
  
  (* from an inv request - used to quickly prompt another inv request *)
  last_expected_block : string;


  blocks_oc : Lwt_io.output Lwt_io.channel ; 

(*
  db : LevelDB.db ; 
*)

}

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


(* useful for debugging *)
let string_of_bytes s = 
  explode s |> List.map (fun ch -> ch |> Char.code |> string_of_int ) |> String.concat " " 



(*
  app state, is a fold over state and events 
  - f s e -> s

  what does one call this state record ?
*)

(*
- jobs = jobs, tasks, threads, fibres
- within a task we can sequence as many sub tasks using >>= as we like
- we only have to thread stuff through this main function, when the app state changes
   and we have to synchronize, 
- app state is effectively a fold over the network events...
*)

let detach f = 
  Lwt_preemptive.detach 
    (fun () -> f ) () 



let f state e =

  (* helpers *)
  let add_jobs jobs state = { state with jobs = jobs @ state.jobs } in

  let remove_peer peer state = { state with 
      (* physical equality *)
      peers = List.filter (fun x -> x.conn.fd != peer.conn.fd) state.peers
    } in
  let add_peer peer state =  { state with peers = peer::state.peers } in

  (* this is horrible, use a Map or something ? 
  let peer_compare a b = a.conn.fd = b.conn.fd  in
  let update_peer new_peer state = { 
    state with peers = List.map (fun peer -> 
     if peer_compare peer new_peer then new_peer else peer 
    )  
  state.peers } in
  *)


  let log a = Lwt_io.write_line Lwt_io.stdout a >> return Nop in
  let format_addr peer = peer.addr ^ " " ^ string_of_int peer.port 

  (* maybe it's the choose() that fails ... *)
(*
  let housekeep state = 
    (* so we need to remove from the connections *)
    let aged conn = (now -. conn.last_activity) > 30. in 
    let stale,ok = List.partition aged state.connections in
    let clean_up_jobs = List.map (fun conn -> 
        log @@ "*** before purging connection " ^ conn.addr 
        >> Lwt_unix.close conn.fd 
        >> log @@ "*** after purging connection " ^ conn.addr 
    ) stale
    in 
    state 
    |> add_jobs clean_up_jobs 
    |> fun state -> { state with connections = ok } 
*)

  in
  let new_state =
    match e with
      | Nop -> state 
      | GotConnection conn ->
        let peer = { conn = conn; block_inv = [] ; block_pending  = false } in
        state
        |> add_peer  peer

        |> add_jobs [
          log @@ "whoot got connection " ^ format_addr conn   ^
            "\npeers now " ^ ( string_of_int @@ List.length state.peers)
          (* or separate? *) 
          >> send_message peer initial_version 
          >> log @@ "*** sent our version " ^ format_addr conn
          ;
           get_message peer 
        ] 
      
      | GotConnectionError msg ->
        add_jobs [ 
          log @@ "could not connect " ^ msg >> return Nop
          ] state 

      | GotMessageError (peer, msg) ->
        state 
        |> remove_peer peer
        |> add_jobs [ 
          log @@ "could not read message " ^ msg
          ^ "\npeers now " ^ ( string_of_int @@ List.length state.peers)
          ;
          Lwt_unix.close peer.conn.fd >> return Nop 
        ]

      | GotMessage (peer, header, raw_header, payload) -> 
        match header.command with
          | "version" ->
            state
            |> add_jobs [ 
              (* should be 3 separate jobs? *)
              log "got version message"
              >> send_message peer initial_verack
              >> log @@ "*** whoot wrote verack " ^ format_addr peer.conn;
              get_message peer 
            ] 

          | "verack" ->
            add_jobs [ 
              (* should be 3 separate jobs? *)
              log "got verack - requesting addr"
              >> send_message peer initial_getaddr ;
              get_message peer 
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
*)
          | "inv" ->
            let _, inv = decodeInv payload 0 in
            (* add inventory blocks to list in peer *)
            let needed_inv_type = 2 in
            let block_hashes = inv
              |> List.filter (fun (inv_type,_) -> inv_type = needed_inv_type )
              |> List.map (fun (_,hash)->hash)
              (* ignore blocks we already have *)
              |> List.filter (fun hash -> not @@ SS.mem hash state.heads )
            in
            remove_peer peer  state
            |> fun state ->  
             let peer = { peer with block_inv = block_hashes @ peer.block_inv  } in
            add_peer peer state  

          |> fun state ->  

            (* code to request a block *) 
            if List.length peer.block_inv > 0 then

              let encodeInventory jobs =
                let encodeInvItem hash = encodeInteger32 needed_inv_type ^ encodeHash32 hash in 
                  (* encodeInv - move to Message  - and need to zip *)
                  encodeVarInt (List.length jobs )
                  ^ String.concat "" @@ List.map encodeInvItem jobs 
              in

              let now = Unix.time () in
              let index = now |> int_of_float |> (fun x -> x mod List.length peer.block_inv) in 
              let hash = List.nth peer.block_inv index in

              let payload = encodeInventory [ hash ] in 
              let header = encodeHeader {
                magic = m ;
                command = "getdata";
                length = strlen payload;
                checksum = checksum payload;
              }
              in
              add_jobs [ 
                log @@ "peer " ^ peer.conn.addr 
                ^ " got " ^ string_of_int (List.length block_hashes ) ^ " inv blocks " 
                ^ " now " ^ string_of_int (List.length peer.block_inv ) 
                   (* ^ "\n" ^ String.concat "\n" (List.map hex_of_string block_hashes)  *)
                    ; 
                (* send_message peer (header ^ payload);  *)
                get_message peer ; 
              ] state (* { state with last_expected_block = last_expected_block }  *)



              else

              state



          |> fun state ->  
              add_jobs [ 
                log @@ "peer " ^ peer.conn.addr 
                ^ " got " ^ string_of_int (List.length block_hashes ) ^ " inv blocks " 
                ^ " now " ^ string_of_int (List.length peer.block_inv ) 
                   (* ^ "\n" ^ String.concat "\n" (List.map hex_of_string block_hashes)  *)
                    ; 
                (* send_message peer (header ^ payload);  *)
                get_message peer ; 
              ] state (* { state with last_expected_block = last_expected_block }  *)


            (* completely separate bit.  code can go anywhere peer.  *)
            |> fun state -> 
            
              (*let now = Unix.time () in
              if now -. state.time_of_last_valid_block  > 60. then 
              *)

              if List.length peer.block_inv < 100 then  
                (*
                  this condition is easy
                    if < 10 (we may be in sync )
                    then only request if sufficient time has elapsed since last time. 

                *)
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
                let now = Unix.time () in
                let index = now |> int_of_float |> (fun x -> x mod List.length heads) in 
                let head = List.nth heads index in
                add_jobs [
                  log @@ "**** update download_head " 
                  ^ "\n download_heads count " ^ (string_of_int @@ List.length heads )
                  ^ "\n head " ^ hex_of_string head 
                  ^ "\n from " ^ format_addr peer.conn   
                  >> send_message peer (initial_getblocks head)
                ]
                (* { state with time_of_last_valid_block = now }   *)
                state
              else
                state
(*
    OK, now we have a single download head that we make requests too...
    we can compute the download heads... in case of fork.
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
            *)

            if not (SS.mem hash state.heads )
              && SS.mem header.previous state.heads then 
              let heads =
                SS.add hash { 
                  previous = header.previous;  
                  height = 0; (* head.height + 1;  *)
                } state.heads
              in
              
              add_jobs [ 
                log @@ "got block - updated chain " 
                  ^ string_of_int  (SS.cardinal heads )
                  ^ " len " ^ (string_of_int (String.length payload));
                Lwt_io.write state.blocks_oc (raw_header ^ payload ) >> return Nop ; 
                get_message peer ; 
              ] { state with heads = heads;  
    
                  time_of_last_valid_block = 
                    if hash = state.last_expected_block then 
                      0.
                    else
                      Unix.time ()

                } 
            else
              add_jobs [ 
                log "got block - already have or cant build on - ignored ";
                get_message peer ; 
              ] state 


          | "tx" ->
            (* at the moment we dont care about tx *)
            (* let _, tx = decodeTx payload 0 in *)
            add_jobs [ 
              (let hash = (payload |> Message.sha256d |> Message.strrev ) in 
              log @@ "got tx!!! "  ^ hex_of_string hash )   ; 
              get_message peer ; 
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
                  get_message peer 
                ] state 
              else 
                add_jobs [ 
                 log @@ "whoot new unknown addr - count "  ^ (string_of_int count ) 
                  ^  " " ^ a ^ " port " ^ string_of_int addr.port ;  
                  get_connection (formatAddress addr) addr.port ;
                  get_message peer
              ] state

          | s ->
            add_jobs [ 
              log @@ "message " ^ s ;
              get_message peer
            ] state

  in new_state 

         
let run f =

  Lwt_main.run (

(*
Lwt_unix.lseek fd pos Unix.SEEK_SET >>= fun pos' ->
assert (pos' = pos);
let block = String.create bs in
_read_buf fd block 0 bs >>= fun () ->
Lwt.return block

      (* let (ic : 'mode Lwt_io.channel )= Lwt_io.of_fd ~mode:Lwt_io.input fd in *)
*)


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

   >>= fun heads ->   
      Lwt_io.write_line Lwt_io.stdout @@ "blocks read " ^ string_of_int (SS.cardinal heads  )

    >>
    (* get initial state up 
      uggh opening the file, non truncate ... 
    *)
(*     Lwt_io.open_file ~flags: [O_WRONLY ] Lwt_io.output       "blocks.dat"  *)  



    Lwt_unix.openfile "blocks.dat"  [O_WRONLY ; O_APPEND ; O_CREAT ] 0  
 
    >>= fun fd -> 
      let blocks_oc = Lwt_io.of_fd ~mode:Lwt_io.output fd  in
 
    (* we actually need to read it as well... as write it... *) 
    let state = 
      let jobs = [
        (* https://github.com/bitcoin/bitcoin/blob/master/share/seeds/nodes_main.txt *)
        get_connection     "23.227.177.161" 8333;
        get_connection     "23.227.191.50" 8333;
        get_connection     "23.229.45.32" 8333;
        get_connection     "23.236.144.69" 8333;
      ] in
(*
      let genesis = string_of_hex "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f" in 
      let heads = 
          SS.empty 
          |> SS.add genesis 
           { 
            previous = ""; 
            height = 0; 
            (* difficulty = 123; *)
          }  in
*)
      { 
        jobs = jobs; 
        peers = []; 
        heads = heads ; 
        time_of_last_valid_block = 0.;  
    (*    db = LevelDB.open_db "mydb"; *)
        last_expected_block = "";

        blocks_oc = blocks_oc
      } 
    in

    let rec loop state =
      Lwt.catch (
      fun () -> Lwt.nchoose_split state.jobs 

        >>= fun (complete, incomplete) ->
          (*Lwt_io.write_line Lwt_io.stdout  @@
            "complete " ^ (string_of_int @@ List.length complete )
            ^ ", incomplete " ^ (string_of_int @@ List.length incomplete)
            ^ ", connections " ^ (string_of_int @@ List.length state.connections )
        >>  
      *)
          let new_state = List.fold_left f { state with jobs = incomplete } complete 
          in if List.length new_state.jobs > 0 then
            loop new_state 
          else
            Lwt_io.write_line Lwt_io.stdout "finishing - no more jobs to run!!" 
            >> return ()
      )
        (fun exn ->
          (* must close *)
          let s = Printexc.to_string exn in
          Lwt_io.write_line Lwt_io.stdout ("finishing - exception " ^ s ) 
          >> (* just exist cleanly *)  
            return ()
        )
    in
      loop state 
  )




let () = run f 



