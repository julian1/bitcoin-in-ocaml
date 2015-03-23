
(* TODO, we need a ping/pong response, to ensure we stay connected

  first bit of script is almost certainly length.
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
   | GotError of string
   | GotReadError of connection * string
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




let getConnection host port =
  (* what is this lwt entry *)
  Lwt_unix.gethostbyname host  (* FIXME this should be in lwt catch as well *)
  >>= fun entry ->
    if Array.length entry.Unix.h_addr_list = 0 then
      return @@ GotError "could not resolve hostname"
    else
      Lwt.catch
        (fun  () ->
        let a_ = entry.Unix.h_addr_list.(0) in
        (* let u = Unix.string_of_inet_addr a_ in *)

        let a = Unix.ADDR_INET ( a_ , port) in 
  
        let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        let (inchan : 'mode Lwt_io.channel )= Lwt_io.of_fd ~mode:Lwt_io.input fd in
        let outchan = Lwt_io.of_fd ~mode:Lwt_io.output fd in

        let conn = {  
            addr = Unix.string_of_inet_addr a_;
            port = port;
            fd = fd;
            ic = inchan ;
            oc = outchan; 
        } in
        Lwt_unix.connect conn.fd a
        >> return @@ GotConnection conn 
        )
        (fun exn ->
          let s = Printexc.to_string exn in
          return @@ GotError s
        )


(* read exactly n bytes from channel, returning a string
  - change name to readn or something? *)
let readChannel inchan length   =
  let buf = Bytes.create length in
  Lwt_io.read_into_exactly inchan buf 0 length
  >>= fun _ ->
    return @@ Bytes.to_string buf


let readMessage conn =
    Lwt.catch (
      fun () -> 
      readChannel conn.ic 24
      (* log *)
    (*   >>= fun s ->
        let _, header = decodeHeader s 0 in
          let _ = Lwt_io.write_line Lwt_io.stdout ("----\n" ^ Message.hex_of_string s ^ "\n" ^ Message.formatHeader header ^ "\n")  in
      return s
    *)
      (* read payload *)
      >>= fun s ->
        let _, header = decodeHeader s 0 in
        readChannel conn.ic header.length
      >>= fun p ->
        return @@ GotMessage ( conn, header, p)
    ) ( fun exn ->  

        (* ok, but what about closing ?? *)
        let s = Printexc.to_string exn in
        return @@ GotReadError (conn, s)
    )

(*
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

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


(* need different lists - connected and not ... *)



type my_app_state =
{
  count : int;
  lst :  myvar Lwt.t list ;
  connections : connection list ; 
}

(* useful for debugging *)
let u s = explode s |> List.map Char.code |> List.map string_of_int |> String.concat " " 

         
let run () =

  Lwt_main.run (
    (*
      app state, is a fold over state and events 
      - f s e -> s
    *)

    (*
    - lst = jobs, tasks, threads, fibres
    - within a task we can sequence as many sub tasks using >>= as we like
    - we only have to thread stuff through this main function, when the app state changes
       and we have to synchronize, 
    - app state is effectively a fold over the network events...
    *)

    let add_job state job = { state with lst = job::state.lst } in

    let format_addr conn = 
      conn.addr ^ " " ^ string_of_int conn.port in 


    let f state e =
      match e with
        | GotConnection conn ->
          { state with
             lst = ( 
                Lwt_io.write_line Lwt_io.stdout ( "whoot got connection " ^ format_addr conn  )
                >> Lwt_io.write conn.oc initial_version
                >> readMessage conn
            ) :: state.lst
; 
            connections = conn :: state.connections 
          }
        
        | GotError msg ->
          add_job state 
          (Lwt_io.write_line Lwt_io.stdout @@ "got error " ^ msg
          >> return Nop
          )
          (* we got a conn error and it all stopped ? *)

        | Nop -> state 

        | GotReadError (conn, msg) ->
          add_job state 
          (Lwt_io.write_line Lwt_io.stdout @@ "got error " ^ msg
          >> return Nop
          ) |> fun state ->
          { state with
            connections = conn :: state.connections 
          } 
 
 

           

        | GotMessage (conn, header, payload) -> 
          match header.command with
            | "version" ->
              add_job state 
              ( Lwt_io.write_line Lwt_io.stdout "version message"
              >> Lwt_io.write conn.oc initial_verack
              >> readMessage conn 
              )

            | "verack" ->
              add_job state 
              ( Lwt_io.write_line Lwt_io.stdout "got verack"
              >> Lwt_io.write conn.oc initial_getaddr (* request addr *) 
              >> readMessage conn 
              )
 

            | "inv" -> 
              add_job state
              (let _, inv = decodeInv payload 0 in
              Lwt_io.write_line Lwt_io.stdout @@ "* got inv " ^ string_of_int (List.length inv) ^ " " ^ format_addr conn  (* ^ formatInv inv *)
              >> readMessage conn 
              )

            | "addr" -> 
              (* it's going to be easiest to stick to common address format 
                int * int * int * int * int
                format...
              
                - there's a race condition here, because we don't update known peers
                here. which is what we should do.
                - irrespective of whether we will connect to them, blacklisted etc, already have enough etc... 
              *)

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
                let already_got = List.exists (fun c -> c.addr = a && c.port = addr.port ) state.connections in
                if already_got then { 
                  state with lst = 
                    (Lwt_io.write_line Lwt_io.stdout 
                    ( "already connected to " ^ a ^ " ignoring " )>> return Nop) 
                  :: readMessage conn  
                  :: state.lst  
                } 
                else { 
                  state with 
                    lst = ( 
                      Lwt_io.write_line Lwt_io.stdout @@ "whoot new unknown addr - count "  ^ (string_of_int count ) 
                      >> Lwt_io.write_line Lwt_io.stdout ( a ^ " port " ^ string_of_int addr.port ) 
                      >> getConnection (formatAddress addr) addr.port 
                      ) 
                      :: readMessage conn  
                      :: state.lst
                    ;
                }

            | s ->
              add_job state
              (Lwt_io.write_line Lwt_io.stdout @@ "message " ^ s
              >> readMessage conn 
              )

    in
    let rec loop state =
      Lwt.nchoose_split state.lst
        >>= fun (complete, incomplete) ->
          Lwt_io.write_line Lwt_io.stdout @@
            "complete " ^ (string_of_int @@ List.length complete )
            ^ ", incomplete " ^ (string_of_int @@ List.length incomplete)
        >>  
          (* loop @@ List.fold_left f incomplete complete  *)
          loop @@ List.fold_left f { state with lst = incomplete } complete 

    in
    let lst = [
         (* getConnection "198.52.212.235"  8333;  *)
      (* http://bitcoin.stackexchange.com/questions/3711/what-are-seednodes *)
   (*     getConnection "24.246.66.189" 8333  *)

        getConnection "dnsseed.bluematt.me"  8333; 
      (*  178.162.19.156 8333 *)
      (* 178.162.19.156 port 8333 
    
      ok so we have an issue that we're connecting to the same address...
       *)
        (*                 68.39.77.241 8333 
          76.121.158.45 8333
       *) 
    ] in

  let state = { count = 123 ; lst = lst; connections = []; } 
  in

    loop state 
  )


let () = run ()



