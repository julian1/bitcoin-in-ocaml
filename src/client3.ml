

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
          log @@ "got connection error " ^ msg >> return Nop
          ] state 

      | GotReadError (conn, msg) ->
        state 
        |> remove_conn conn
        |> add_jobs [ 
          log @@ "got error " ^ msg
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
              |> List.filter (fun (inv_type,hash) -> inv_type == 1)
              |> List.map (fun (_,hash)->hash)
            in
            let encodeInventory lst =
              (* encodeInv - move to Message  - and need to zip *)
              encodeVarInt (List.length lst )
              ^ String.concat "" 
                (List.map (fun hash -> encodeInteger32 1 ^ encodeHash32 hash) lst)
            in
            let payload = encodeInventory block_hashes in 
            let header = encodeHeader {
              magic = m ;
              command = "getdata";
              length = strlen payload;
              checksum = checksum payload;
              } 
            in
            add_jobs [ 
              send_message conn (header ^ payload) ; 
              log @@ "* got inv " ^ string_of_int (List.length inv) ^ " " ^ (format_addr conn) ;
              get_message conn ; 
            ] state


            | "tx" ->
              (* at the moment we dont care about tx *)
              let _, tx = decodeTx payload 0 in
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
          Lwt_io.write_line Lwt_io.stdout  @@
            "complete " ^ (string_of_int @@ List.length complete )
            ^ ", incomplete " ^ (string_of_int @@ List.length incomplete)
            ^ ", connections " ^ (string_of_int @@ List.length state.connections )
        >>  
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



