

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

type myvar =
   | GotConnection of connection
   | GotMessage of connection  * header * string
   | GotConnectionError of string
   | GotReadError of connection * string (* change name MessageError - because *)
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


let get_message conn =
    Lwt.catch (
      fun () -> 
      (* read header *)
      readChannel conn.ic 24
      >>= fun s ->
        let _, header = decodeHeader s 0 in
        if header.length < 10*1000000 then
          (* read payload *)
          readChannel conn.ic header.length
          >>= fun p -> 
          return @@ GotMessage ( conn, header, p)
        else
          return @@ GotReadError (conn, "payload too big" ) 
      ) 
      ( fun exn ->  
          let s = Printexc.to_string exn in
          return @@ GotReadError (conn, "here2 " ^ s) 
      )
       

let send_message conn s = 
    Lwt_io.write conn.oc s >> return Nop 


(*


          (* do we have to close the channels as well as the descriptor?? *)
          Lwt_unix.close conn.fd 
          >>

let filterTerminated lst =
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
  List.filter f lst
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
type my_app_state =
{
  count : int;
  lst :  myvar Lwt.t list ;
  connections : connection list ; 
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
- lst = jobs, tasks, threads, fibres
- within a task we can sequence as many sub tasks using >>= as we like
- we only have to thread stuff through this main function, when the app state changes
   and we have to synchronize, 
- app state is effectively a fold over the network events...
*)

let f state e =

  (* helpers *)
  let add_jobs jobs state = { state with lst = jobs @ state.lst } in
  let remove_conn conn state = { state with 
      (* physical equality *)
      connections = List.filter (fun x -> x.fd != conn.fd) state.connections
    } in
  let add_conn conn state =  { state with connections = conn::state.connections } in
  let log a = Lwt_io.write_line Lwt_io.stdout a >> return Nop in
  let format_addr conn = conn.addr ^ " " ^ string_of_int conn.port 

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
        state
        |> add_conn conn
        |> add_jobs [
          log @@ "whoot got connection " ^ format_addr conn   ^
            "\nconnections now " ^ ( string_of_int @@ List.length state.connections)
          (* or separate? *) 
          >> send_message conn initial_version 
          >> log @@ "*** whoot wrote initial version " ^ format_addr conn
          ;
           get_message conn
        ] 
      
      | GotConnectionError msg ->
        add_jobs [ 
          log @@ "could not connect " ^ msg >> return Nop
          ] state 

      | GotReadError (conn, msg) ->
        state 
        |> remove_conn conn
        |> add_jobs [ 
          log @@ "could not read message " ^ msg
          ^ "\nconnections now " ^ ( string_of_int @@ List.length state.connections)
          ;
          Lwt_unix.close conn.fd >> return Nop 
        ]

      | GotMessage (conn, header, payload) -> 
        match header.command with
          | "version" ->
            state
            |> add_jobs [ 
              (* should be 3 separate jobs? *)
              log "got version message"
              >> send_message conn initial_verack
              >> log @@ "*** whoot wrote verack " ^ format_addr conn
              ;
              get_message conn 
            ] 

          | "verack" ->
            add_jobs [ 
              (* should be 3 separate jobs? *)
              log "got verack - requesting addr"
              >> send_message conn initial_getaddr
              ;
              get_message conn 
            ] state 

(*          | "inv" -> 
            let _, inv = decodeInv payload 0 in
            add_jobs [ 
              log @@ "* got inv " ^ string_of_int (List.length inv) ^ " " ^ (format_addr conn)
              ;
              get_message conn 
            ] state
*)

          | "inv" ->
            let _, inv = decodeInv payload 0 in
            let block_hashes = inv
              |> List.filter (fun (inv_type,_) -> inv_type == 2)
              |> List.map (fun (_,hash)->hash)
            in
  
            (* - want to also filter in leveldb 
              but this is an IO action  
              - which needs to be encoded as a job...
              -- actually it maybe simple just do a di
              -----------
              - the sequence actually (normally) begins with our request for future blocks
                  and this will determine who we get the responses from...
              - then we get inv of 500? 
              - then we will request in order...
              - and what about if two different nodes have a different view 
  
              - ok, to pick up forks we really need to request future blocks
              from all peers.
              - don't merge, - because we need to know what peer has what 
                - actually we could merge into a Map hash -> List of peers.
                - no cause then we loose the ordering.
              - and work our way through them...
              - testing if we already have the block...
              -----------------------
  
              - each individual peer - only has a linear chain because it knows it's best path.     
                  but the network might have a tree.
                - our job is to ensure we get the tree, so we can verify the best path ourselves.
                - we know the root node (genesis)
 
              - 1. if a head hasn't updated for a period (5 mins) send message request for next to to a                 all peers 
                  (so we don't loose forks)
              - 2. we'll get back inventory of next 500 blocks back, from all peers 
                    - then group for each sequence of hashes,taking a certain number (10,100,500) etc. 
                    WE HAVE TO GROUP to know we explored all fork options
                    
              - 3. if we havent seen a returned group before - request the block data from that peer 
                  otherwise if we have seen (meaning we've issued request), ignore.

              - 4. if a particular head doesn't move for 5 minutes then we just issue again.


                - first node in the list represents the last valid block we have.  
                [[]]]

                **********************
                SIMPLER APPROACH - would be just select single next item and request it.
                    then we don't group.

                heads (gets updated as we get new blocks pointing back)
                  [ genesis ] k 
                  - after block inv request...

                pending sequences (just an optimisation to prevent 
                  downloading the same thing from multiple sources - but why not make finegrained)
                  - might clear this on the timer. will need to be a map.
   
                we don't need received, it's implied by heads. we kind of do for out-of-order
                spurious inv messages. will be in leveldb
 
                  ----
                  - look at the heads and request next sequence of blocks from all peers.

                  - when get a inv sequence returned from a peer - select top 10 as seq and
                check if not already pending.
                  (we have to select everything to avoid arriving out-of-order) 
                  - so it has to be a sequence and we request everything in the sequence in 
                  order, so the peer can give us something.

                  - we don't remove from pending - except on fallback timing. kkkkkkk  

                - so it's very little different from what we have already. except we ask from
                  everyone. then filter to avoid requesting the same thing too often.  

                - VERY IMPORTANT - and to keep it ticking over after we've got our 100 blocks
                or so, - we can just mark in the pending that when we receive that block
                we should do more head requests.

                  - when we get a block we can remove it from pending. no because we want to 
                  prevent another

                **********************

                  [ hash0 hash1 hash2 hash3 ]  
                  [ hash0 hash1 hash4 hash5 ]  
                  - so for each of those sequences we try to pull them

                - when we get hash0 we see it links to genesis, so we remove genesis.
                - when we get hash1 we'll remove hash0 from both lists. 

                - we'll only save a block to disk if it advances a known head. but we can't avoid
                downloading it, since we have to check if it's a valid fork

                - ok the spurious inv we get are not a problem . we add to the list of segments,
                do the download, check and remove 

                - we have to have two lists. the actual heads. 

            -----

                IMPORTNAT - if someone sends us an invalid block to get in an inv, we still have to
                retrieve it.  where headers first we dont because we can validate it.
                - but... we can drop the connection after a bit. but this is complicated.


                IMPORTANT maybe get rid of the current head. and instead use a list of size one. when
                we're at a tip.
  
                [ hash, hash hash hash ]
                [ hash ]  <-- at tip


              - IMPORTANT - we need to make sure we can immediately continue when we have our block. 
                  easy. just test the received block with the last in the set. in which 
                  case we'll go back to 2. 

              - we deal in sequences of hashes which makes it easier - not the next 1. 
                but the next ten or 100 .
                ie. have we sent a request for the next 100 blocks from x hash.
                  - if yes then don't request again.
            *) 

            add_jobs [ 
              (* check if pending or already have and request if not 
                don't bother to format message unless we've got.
                also write that it's pending...
              *)
              if List.length block_hashes > 0 then
                let encodeInventory lst =
                  (* encodeInv - move to Message  - and need to zip *)
                  encodeVarInt (List.length lst )
                  ^ String.concat "" 
                    (List.map (fun hash -> encodeInteger32 2 ^ encodeHash32 hash) lst)
                in
                let payload = encodeInventory block_hashes in 
                let header = encodeHeader {
                  magic = m ;
                  command = "getdata";
                  length = strlen payload;
                  checksum = checksum payload;
                  } 
                in
                log @@ "requesting block (actually not) " 
                  ^ string_of_int (List.length block_hashes )^ " " 
                  ^ hex_of_string (List.hd block_hashes) ^ " " 
                  ^ conn.addr ^ " " 
                  ^ string_of_int conn.port  
                (* >> send_message conn (header ^ payload) *)
              else
                return Nop ;
              
              (* log @@ conn.addr ^ " " ^ string_of_int conn.port ^ " got inv !!! " ;  *)
              get_message conn ; 
            ] state

          | "block" ->
            (* let _, block = decodeBlock payload 0 in *)
            add_jobs [ 
              log @@ "got block " ^ conn.addr ^ " " ^ string_of_int conn.port  ; 
              get_message conn ; 
            ] state


          | "tx" ->
            (* at the moment we dont care about tx *)
            (* let _, tx = decodeTx payload 0 in *)
            add_jobs [ 
              log "got tx!!! " (*^ ( Message.formatTx tx) *); 
              get_message conn ; 
            ] state


          | "addr" -> 
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
              let already_got = List.exists (fun c -> c.addr = a && c.port = addr.port ) 
                  state.connections 
              in
              if already_got || List.length state.connections > 30 then  
                add_jobs [ 
                  log @@ "whoot new addr - already got or ignore " ^ a  
                  ;
                  get_message conn  
                ] state 
              else 
                add_jobs [ 
                 log @@ "whoot new unknown addr - count "  ^ (string_of_int count ) 
                  ^  " " ^ a ^ " port " ^ string_of_int addr.port 
                 ;  
                  get_connection (formatAddress addr) addr.port 
                  ;
                  get_message conn  
              ] state

          | s ->
            add_jobs [ 
              log @@ "message " ^ s
              ;
              get_message conn 
            ] state

  in new_state 

         
let run f s =
  Lwt_main.run (
    let rec loop state =
      Lwt.catch (
      fun () -> Lwt.nchoose_split state.lst

        >>= fun (complete, incomplete) ->
          (*Lwt_io.write_line Lwt_io.stdout  @@
            "complete " ^ (string_of_int @@ List.length complete )
            ^ ", incomplete " ^ (string_of_int @@ List.length incomplete)
            ^ ", connections " ^ (string_of_int @@ List.length state.connections )
        >>  
*)
          let new_state = List.fold_left f { state with lst = incomplete } complete 
          in if List.length new_state.lst > 0 then
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

(* - ok there's an easy way to handle this 
  - we have a read bound.
  - what we need is to put it on a timer so it returns...
  after a period.
  - then we ought to be able to close it 
  - it may well be easier... since only need to deal with one at a time...

  kk
*)

    in
    loop s 
  )


let s = 
  let lst = [
    (* https://github.com/bitcoin/bitcoin/blob/master/share/seeds/nodes_main.txt *)
    get_connection     "23.227.177.161" 8333;
    get_connection     "23.227.191.50" 8333;
    get_connection     "23.229.45.32" 8333;
    get_connection     "23.236.144.69" 8333;
  ] in
  { count = 123 ; lst = lst; connections = []; } 

let () = run f s  



