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
  (* get rid of this,  sockets don't need buffers *) 
  ic : Lwt_io.input Lwt_io.channel ; 
  oc : Lwt_io.output Lwt_io.channel ; 
}


type my_event =
   | GotConnection of connection
   | GotConnectionError of string
   | GotMessage of connection * header * string * string
   | GotMessageError of connection * string (* change name MessageError - because *)
   | Nop


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

        let ic = conn.ic in 
        readChannel ic 24
        >>= fun s ->
          let _, header = decodeHeader s 0 in
          if header.length < 10*1000000 then
            (* read payload *)
            readChannel ic header.length
            >>= fun p -> 
            return @@ GotMessage ( conn, header, s, p)
          else
            return @@ GotMessageError (conn, "payload too big - command is " 
              ^ header.command 
              ^ " size " ^ string_of_int header.length ) 
      ) 
      ( fun exn ->  
          let s = Printexc.to_string exn in
          return @@ GotMessageError (conn, "here2 " ^ s) 
      )
       

let send_message conn s = 
    let oc = conn.oc in
    Lwt_io.write oc s >> return Nop  (* message sent *)




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


type my_app_state =
{
  jobs :  my_event Lwt.t list ;

  connections : connection list ; 

  heads : my_head SS.t ;  

  time_of_last_received_block : float; 
  time_of_last_inv_request : float; 
  requested_blocks : string list ; 

   (* should change to be blocks_fd 
      does this file descriptor even need to be here. it doesn't change?
    *)
(*  blocks_oc : Lwt_io.output Lwt_io.channel ; *) 
  (* db : LevelDB.db ; *)
}


(*
- jobs = jobs, tasks, threads, fibres
- within a task we can sequence as many sub tasks using >>= as we like
- we only have to thread stuff through this main function, when the app state changes
   and we have to synchronize, 
- app state is effectively a fold over the network events...
*)

let log a = Lwt_io.write_line Lwt_io.stdout a >> return Nop 


let format_addr conn = 
  let s = conn.addr ^ ":" ^ string_of_int conn.port in 
  Misc.pad s 18 
     

(* 50.199.113.193:8333 *)

(*
let format_addr conn = String.concat "" [ conn.addr ; ":" ; string_of_int conn.port ] 
*)



let f state e =

  match e with
    | Nop -> state 
    | GotConnection conn ->
      { state with 
        connections = conn :: state.connections;
        jobs = state.jobs @ [  
          log @@ format_addr conn ^  " got connection "  ^
            ", connections now " ^ ( string_of_int @@ List.length state.connections )
          >> send_message conn initial_version 
          >> log @@ "*** sent our version " ^ format_addr conn
          ;
           get_message conn
        ];
      }

    | GotConnectionError msg ->
      { state with
        jobs = state.jobs @ [  log @@ "connection error " ^ msg ] 
      }

    | GotMessageError (conn, msg) ->
      { state with
        (* fd test is physical equality *)
        connections = List.filter (fun c -> c.fd != conn.fd) state.connections;
        jobs = state.jobs @ [  
          log @@ format_addr conn ^ "msg error " ^ msg; 
          match Lwt_unix.state conn.fd with
            Opened -> ( Lwt_unix.close conn.fd ) >> return Nop 
            | _ -> return Nop 
        ] 
      }


    | GotMessage (conn, header, raw_header, payload) -> 
      (
      match header.command with

        | "version" ->
          { state with 
            jobs = state.jobs @ [  
              log @@ format_addr conn ^ " got version message"
              >> send_message conn initial_verack
              >> log @@ "*** sent verack " ^ format_addr conn
              ;
              get_message conn 
            ];
          }

        | "verack" ->
          { state with 
            jobs = state.jobs @ [ 
              (* should be 3 separate jobs? *)
              log @@ format_addr conn ^ " got verack";
              (* >> send_message conn initial_getaddr *)
              get_message conn 
            ]
          }

        (* - there's stuff to manage p2p 
            - then there's stuff to manage chainstate.. 
            - can we separate this out into another file...
            - blocks and inv etc. 

  
            - we could actually just have a complete module ...
            with msg interface...
              - hiding the blocks and db file descriptors etc...
        *)


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
            (* ignore, same addr instances on different ports *)
            let already_got = List.exists (fun c -> c.addr = a (* && peer.conn.port = addr.port *) ) state.connections
            in
            if already_got || List.length state.connections >= 30 then  
              { state with 
                jobs = state.jobs @ [ 
                  log @@ format_addr conn ^ " addr - already got or ignore " 
                    ^ a ^ ":" ^ string_of_int addr.port ;
                  get_message conn 
                  ]
                }
            else 
              { state with  
              jobs = state.jobs @ [ 
                 log @@ format_addr conn ^ " addr - count "  ^ (string_of_int count ) 
                    ^  " " ^ a ^ " port " ^ string_of_int addr.port ;  
                  get_connection (formatAddress addr) addr.port ;
                  get_message conn 
                ]  
              }

        | s ->
          { state with  
            jobs = state.jobs @ [
              log @@ format_addr conn ^ " message " ^ s ;
              get_message conn 
              ]
          }

        )
 
         
let run f =

  Lwt_main.run (

 
    (* we actually need to read it as well... as write it... *) 
    let state = 
      let jobs = [
        (* https://github.com/bitcoin/bitcoin/blob/master/share/seeds/nodes_main.txt *)
        get_connection     "23.227.177.161" 8333; 
        get_connection     "23.227.191.50" 8333;
        get_connection     "23.229.45.32" 8333;
        get_connection     "23.236.144.69" 8333;
        
        get_connection     "50.142.41.23" 8333; 
        get_connection     "50.199.113.193" 8333; 
        get_connection     "50.200.78.107" 8333; 
  

        get_connection     "61.72.211.228" 8333; 
        get_connection     "62.43.40.154" 8333; 
        get_connection     "62.43.40.154" 8333; 
        get_connection     "62.80.185.213" 8333; 





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
        connections = []; 
        heads = SS.empty ; 

        time_of_last_received_block = 0. ; 
        time_of_last_inv_request = 0.; 
        requested_blocks  = [ ] ; 

    (*    db = LevelDB.open_db "mydb"; *)

      (*  blocks_oc = blocks_oc *)
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


          let s = Printexc.to_string exn  ^ "\n" ^ (Printexc.get_backtrace () ) in
          Lwt_io.write_line Lwt_io.stdout ("finishing - exception " ^ s ) 
          >> (* just exist cleanly *)  
            return ()
        )
    in
      loop state 
  )




let () = run f 



