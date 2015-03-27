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



module SS = Map.Make(struct type t = string let compare = compare end)

(* change name to requested *)
type pending_request =
{
    addr1 : string  ; (* or conn? *)
}



type myblock = 
{
    hash : string;
    previous : string;  (* could be a list pointer at myblock *) 
    height : int;   (* if known? *)
    difficulty : int ; (* aggregated *)
    (* bool requested *)
    (* bool have *)

    last_request : float;
}



type my_app_state =
{
  count : int;
  lst :  myvar Lwt.t list ;
  connections : connection list ; 

	pending : pending_request SS.t;	


	(* heads : myblock SS.t;	*)
	heads : myblock list ;	

  db : LevelDB.db ; 

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

let detach f = 
  Lwt_preemptive.detach 
    (fun () -> f ) () 



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

          (* - ok, so for each block we have to mark in pending 
            - and we want to limit the number
              - we might use a map record from who it's pending to make it easier to work with?
              - or the request time
          *)

          (*
            when analyzing the heads. 
              - make a request...  if the block is not pending. 
              - and put the time of request in the pending so we can see if 
                  it was ever received.
      
              - actually we broadcast our getall blocks to all nodes...

              update of pending is easy. if it's not received and pending shows it's
              stale, then delete the entry from pending.

              - are we sure we don't want to combine the head. 
          *)
          (*
			- server only sent half the blocks and stopped. 
			- requesting again ... 
			- but blocks were marked as pending already so they're ignored , so 
 
			- when cleared pending we advanced furtuer 
          *)

		(*	
			**** VERY IMPORTANT ****
			if something is pending and isn't received after a minute, then 
				we should clear it. pending can just be set to 30 secs or something...
				to cover time between request and receive 
				we do need the recieved list however.

			- OK, we're probably getting 244, because of some truncation somewhere in how we're 
			encoding the varInt request ... 
			- if we can mark expected we're much better.
			then we can mark the last item - so that we can immediately request again.
	
			- actually we could test that pending == received
			- no just keep a last item.
		*)

	        (*
            - actually the first thing we should do - is filter inv items
            against leveldb.     
            - either thread this through the main block
            - ... 
            - there's actually no reason why why can't run this through the 
              job ...
              leveldb -> have we got.
              no send message.
-> -> 
          *)	
          | "inv" ->
            let needed_inv_type = 2 in

            let _, inv = decodeInv payload 0 in
            let block_hashes = inv
              |> List.filter (fun (inv_type,_) -> inv_type = needed_inv_type )
              |> List.map (fun (_,hash)->hash)
            in

            if List.length block_hashes > 0 then

              (* - ok, all this stuff wants to be moved into the job and only
                done for items that don't exist in level db 
                - but leveldb interface - expects one at a time? 
                - no we can pass a list, and get back key/ responses
              *)
				add_jobs [ 

                  (* - ok, we want to batch the jobs somehow - cause we have to get a list 
                    at the end. is there something like a fold? 
                    - even if we manage to get it to join(). we're not going to get 
                    an aggregated response at the end 
                    - ok, join may not be any good but choose will hoose one
                    - pumping it non sequentially is going to loose the ordering...
                    (input, output ) jj

					- hang on we should be able to do a left fold... anyway
                    List.fold_left (fun acc, x -> (detach @@ LevelDB.mem state.db x) :: acc ) block_hashes 
						no it's not going to expose it. we need to bind >> the results
						through... 
                  *) 
                 ( 
                  Lwt.choose ( 
                    List.map (fun hash -> detach @@ LevelDB.mem state.db hash ) block_hashes 
                   ) 
                    >>= fun v -> return Nop 
                  ) ;
				(                                  
					detach @@ 
					List.fold_left (fun acc hash 
						-> if LevelDB.mem state.db hash then acc else hash::acc ) [] block_hashes
					>>= fun block_hashes ->  
					   let encodeInventory lst =
							let encodeInvItem hash = encodeInteger32 needed_inv_type ^ encodeHash32 hash in 
							(* encodeInv - move to Message  - and need to zip *)
							encodeVarInt (List.length lst )
							^ String.concat "" @@ List.map encodeInvItem lst
						  in
						  let payload = encodeInventory block_hashes in 
						  let header = encodeHeader {
							magic = m ;
							command = "getdata";
							length = strlen payload;
							checksum = checksum payload;
						  }
						in 

						log @@ "request " ^ String.concat "\n" (List.map hex_of_string block_hashes) 
						>> send_message conn (header ^ payload); 
				)
					;
		(*
                  (detach @@ LevelDB.mem state.db "hash" >>= fun _ ->  return Nop ); 
                  log @@ "request " ^ String.concat "\n" (List.map hex_of_string block_hashes) ; 
                  (* send_message conn (header ^ payload); *) 
		*)
                  get_message conn ; 
                ] state 

            else
              add_jobs [ 
                get_message conn ; 
              ] state

            (* completely separate bit.
                can be tacked on to any message response from a peer.
                actually this code, is nice in how it round-robbins
             *)
            |> fun state -> 

              let now = Unix.time () in
              let stale_test (x : myblock) = (now -. x.last_request) > 10. in
              let stale,ok = List.partition stale_test state.heads in 
              (* update timestamp *)
              let stale = List.map (fun x -> { x with last_request = now; } ) stale in
              let state = { state with heads = ok @ stale } in 
              let jobs = List.map (fun x -> 
                  log @@ " requesting head hash " ^ hex_of_string x.hash
                  ^ " from " ^ format_addr conn   
                  ^ " length heads " ^ string_of_int @@ List.length state.heads 
                  >> send_message conn (initial_getblocks x.hash)
              ) stale in 
              add_jobs jobs 
                 state 



              (* check if pending or already have and request if not 
                don't bother to format message unless we've got.
                also write that it's pending...

      let _, header = decodeBlock payload 0 in 

      Lwt_io.write_line Lwt_io.stdout ( "* got block " ^ ( Message.hex_of_string hash) 
        ^ " previous " ^  Message.hex_of_string header.previous ^ "\n" )

      (* find() will throw if entry not found...  *)
      >> 
      (* does this block point at a head - if so we update the head! *)
      if SS.mem header.previous state.heads then 
        let old = SS.find header.previous state.heads in 
        let new_ = { hash = hash; previous = header.previous; height = old.height + 1 ; difficulty = 123 } in 
        let state = { 
          state with 

 
              *)

              
              (* log @@ conn.addr ^ " " ^ string_of_int conn.port ^ " got inv !!! " ;  *)

          | "block" ->
            (* let _, block = decodeBlock payload 0 in *)
            let hash = (Message.strsub payload 0 80 |> Message.sha256d |> Message.strrev ) in
            let _, header = decodeBlock payload 0 in 

            (* should be just remove and add, which is a map ... *)
            let new_heads = List.map (fun x -> 
              if header.previous = x.hash then
                { 
                  hash = hash; 
                  previous = header.previous;  
                  height = x.height + 1; 
                  last_request = Unix.time ();
                  difficulty = x.difficulty 
              }  
              else x 
            ) state.heads in 
 
            add_jobs [ 
              log @@ "got " 
				^ " block " ^ hex_of_string hash 
				^ " from " ^ conn.addr ^ " " ^ string_of_int conn.port 
                ^ " heads count " ^ string_of_int (List.length state.heads) 
                ^ " heads " ^ String.concat " " @@ List.map ( fun x -> 
					hex_of_string x.hash
					^ " height " ^ string_of_int x.height 
				) 
				new_heads ; 

              get_message conn ; 
            ] { state with heads = new_heads } 


          | "tx" ->
            (* at the moment we dont care about tx *)
            (* let _, tx = decodeTx payload 0 in *)
            add_jobs [ 
              (let hash = (payload |> Message.sha256d |> Message.strrev ) in 
              log  "got tx!!! " (* ^ hex_of_string hash *) )   ; 
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
  let heads = 
      (* as an exception - we add genesis even though it's not been downloaded it yet *)
      [ { 
        hash = string_of_hex "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"; 
        previous = ""; 
        height = 0; 
        difficulty = 123; 
        last_request = Unix.time (); (* now *)
      } ] in
  { 
    count = 123 ; 
    lst = lst; 
    connections = []; 
    pending = SS.empty ; 
    heads = heads ; 
    db = LevelDB.open_db "mydb"; 
  } 

let () = run f s  



