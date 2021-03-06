
let (>>=) = Lwt.(>>=)
let return = Lwt.return

module M = Message
module S = String
module L = List

module U = Util
module PG = U.PG

let (<|) f g x = f(g(x))

let fold_m f acc lst =
  let adapt f acc e = acc >>= fun acc -> f acc e in
  L.fold_left (adapt f) (return acc) lst



let log s = U.write_stdout s >> return U.Nop


(* initial version message to send 
  TODO change name encode_version_message
*)
let initial_version network =
  let payload = M.encodeVersion {

      (* protocol = 70002;  bitcoin/litecoin  match network with ... *)
      protocol = (match network with
        | M.Bitcoin | M.Litecoin -> 70002
        | M.Dogecoin -> 70003
        ) ;

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
  M.encodeMessage network "version" payload

(* verack response to send TODO get rid of these one line functions *)
let initial_verack network =
  M.encodeSimpleMessage network "verack"


let initial_getaddr network =
  M.encodeSimpleMessage network "getaddr"


let close_fd fd =
  match Lwt_unix.state fd with
    Opened -> ( Lwt_unix.close fd ) >> return U.Nop
    | _ -> return U.Nop



let get_connection host port =
  (* what is this lwt entry *)
  Lwt_unix.gethostbyname host  (* FIXME this should be in lwt catch as well *)
  >>= fun entry ->
    if Array.length entry.Unix.h_addr_list = 0 then
      return @@ U.GotConnectionError ( "could not resolve hostname " ^ host )
    else
      let a_ = entry.Unix.h_addr_list.(0) in
      let a = Unix.ADDR_INET ( a_ , port) in
      let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let (inchan : 'mode Lwt_io.channel )= Lwt_io.of_fd ~mode:Lwt_io.input fd in
      let outchan = Lwt_io.of_fd ~mode:Lwt_io.output fd
      in
      Lwt.catch
        (fun  () ->
          Lwt.pick [
            Lwt_unix.timeout 20.
            (* >> Lwt_io.write_line Lwt_io.stdout "conn timeout!!!" doesn't run *)
            ;
            Lwt_unix.connect fd a
            >>
            let (conn : U.connection) = {
                addr = Unix.string_of_inet_addr a_;
                port = port;
                fd = fd;
                ic = inchan ;
                oc = outchan;
            } in
            return @@ U.GotConnection conn
          ]
        )
        (fun exn ->
          (* must close *)
         close_fd fd
          >>
          let s = Printexc.to_string exn in
          return @@ U.GotConnectionError s
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



let get_message (conn : U.connection ) =
    Lwt.catch (
      fun () ->
      (* read header *)

        let ic = conn.ic in
        readChannel ic 24
        >>= fun s ->
          let _, header = M.decodeHeader s 0 in
          if header.length < 10*1000000 then
            (* read payload *)
            readChannel ic header.length
            >>= fun p ->
            return @@ U.GotMessage ( conn, header, s, p)
          else
            return @@ U.GotMessageError (conn, "payload too big - command is "
              ^ header.command
              ^ " size " ^ string_of_int header.length )
      )
      ( fun exn ->
          let s = Printexc.to_string exn in
          return @@ U.GotMessageError (conn, "here2 " ^ s)
      )

(*
- jobs = jobs, tasks, threads, fibres
- within a task we can sequence as many sub tasks using >>= as we like
- we only have to thread stuff through this main function, when the app state changes
   and we have to synchronize,
- app state is effectively a fold over the network events...
*)




(* hmmmmm

  - IMPORTANT rather than keep complicated pending count. just do it on 1 minute intervals ... 
      first is to try to get connection...

  - problem is peers that may send data (and therefore don't get closed), but never complete the handshake...

  - we could return the time ...

type string_array = string option list
val string_of_string_array : string_array -> string
v

*)

let manage_p2p2 state e =
  let state = (state : U.my_app_state) in

    log @@ "*** whoot pending " ^ string_of_int state.pending_connections
    >> 
    let required = 8 - (L.length state.connections + state.pending_connections) in 
    if required > 0 then
      (* need to insert current conns to get unique
        select id,addr,port from peer where addr not in ( '49.116.148.241', '37.49.9.64' ) order by random();
        ok, somehow we need pending. Ahhh why not test if the queue is empty???? 

        - may stall if no verack ? no it will get cleaned out...
        - we kind of also want to avoid reconnecting to pending... 
      *)
      log "\n*****************\n db lookup peers"

      >> PG.prepare state.db "select addr,port from peer where not ( addr = any ( $2))  order by random() limit $1" ()
      (* >> PG.prepare state.db "select addr,port from peer where addr not in (select unnest( $2) ) order by random() limit $1" () *)
      >> PG.execute state.db ~params:[
          Some (PG.string_of_int required  );
          let lst = L.map U.(fun conn -> Some conn.addr) state.connections in
          Some (U.PG.string_of_string_array lst )
         ] ()
      >>= fun rows ->
        log @@ " got " ^ (string_of_int <| L.length) rows 
      >>
        let lst = L.map (function | (Some addr :: Some port :: []) -> 
          PG.string_of_string addr, PG.int_of_string port) rows 
        in

        fold_m (fun acc (addr,port) -> U.write_stdout @@ "trying addr " ^ addr ^ " " ^ string_of_int port) () lst 

      >>
        let jobs = L.map (fun (addr,port) -> get_connection addr port ) lst in 
        let state = { state with pending_connections = state.pending_connections + L.length lst } in
        return @@ U.SeqJobFinished (state, jobs)
    else
      return @@ U.SeqJobFinished (state, [])



(* let update state e =  *)
let manage_p2p1 state e =
  let state = (state : U.my_app_state) in
  match e with
    | U.Start ->
      return (U.SeqJobFinished (state, []))

    | U.GotConnection conn ->
      log "whoot got connection"
      >>
      let jobs = [
        log @@ U.format_addr conn ^  " got connection "
        >> U.send_message conn (initial_version state.network)
        >> log @@ "*** sent our version " ^ U.format_addr conn;
        get_message conn
      ]
      in
      return (U.SeqJobFinished (state, jobs))

    | U.GotConnectionError msg ->
      let jobs = [ log @@ "connection error " ^ msg ] in

      let state = { state with pending_connections = abs( pred state.pending_connections ) } in 
      return (U.SeqJobFinished (state, jobs))

    | U.GotMessageError (conn , msg) ->
      (* fd test is physical equality *)
      let state = { state with
        connections = List.filter (fun (c : U.connection) -> c.fd != conn.fd) state.connections;
      } in
      let jobs = [
        log @@ U.format_addr conn ^ "msg error " ^ msg;
        (* TODO move this outside jobs ??? *)
        close_fd conn.fd;
      ] in
      let state = { state with pending_connections = abs ( pred state.pending_connections) } in 
      return @@ U.SeqJobFinished (state, jobs)


    | U.GotMessage (conn, header, raw_header, payload) ->
      match header.command with
          
        | "version" ->
          log @@ U.format_addr conn ^ " got version ";
          >> 
            let jobs = [
              U.send_message conn (initial_verack state.network)
              ; 
              get_message conn
            ] in
            return @@ U.SeqJobFinished (state, jobs)


        | "verack" ->
          (* handshake complete *)
          log @@ U.format_addr conn ^ " got verack message"
          >>
          (* record peer if not recorded *)
            PG.prepare state.db "insert into peer(addr,port) select $1, $2 where not exists ( select 1 from peer where addr = $1) " ()
          >> PG.execute state.db ~params:[
              Some (PG.string_of_bytea conn.addr );
              Some (PG.string_of_int conn.port );
            ] ()
          >>
            let state, jobs =
              if L.length state.connections < 8 then
                { state with
                  connections = conn :: state.connections;
                  pending_connections = abs( pred state.pending_connections ) ; 
                } ,
                [
                  log @@ "*** adding conn " ^ U.format_addr conn
                  >> U.send_message conn (initial_getaddr state.network) 
                  >> get_message conn
                ]
              else
                { state with
                  pending_connections = abs( pred state.pending_connections ); 
                }, 
                [
                    close_fd conn.fd
                    >> log @@ "*** dropping conn " ^ U.format_addr conn
                ]
            in
            return @@ U.SeqJobFinished (state, jobs)


        | "addr" ->
            (* on unknown peer - try to make a connection to a new peer *)

            log @@ U.format_addr conn ^ " got addr message"
            >>

            let pos, count = M.decodeVarInt payload 0 in
            (* should take more than the first *)
            let pos, _ = M.decodeInteger32 payload pos in (* timeStamp  *)
            let _, addr = M.decodeAddress payload pos in
            let formatAddress (h : M.ip_address ) =
              let soi = string_of_int in
              let a,b,c,d = h.address  in
              S.concat "." [
              soi a; soi b; soi c; soi d
              ] (* ^ ":" ^ soi h.port *)
            in
            let a = formatAddress addr in
            (* ignore, same addr instances on different ports - hmmm we should look up in db first *)

            PG.prepare state.db "select exists ( select 1 from peer where addr = $1) " ()
            >> PG.execute state.db ~params:[
                Some (PG.string_of_bytea a );
              ] ()
            >>= fun rows ->
              let already_have = match rows with
                | (Some "t"::[])::_ -> true
                | (Some "f"::[])::_ -> false
            in
            (* let already_have = L.exists (fun (c : U.connection) -> c.addr = a (* && peer.conn.port = addr.port *) ) state.connections
            in *)
            if already_have (* || L.length state.connections >= 8 *) then
              let jobs = [
                  log @@ U.format_addr conn ^ " addr - count "  ^ (string_of_int count) ^ " already got conn " ^ a ^ ":" ^ string_of_int addr.port ;
                  get_message conn
              ] in
              return @@ U.SeqJobFinished (state, jobs)
            else
              let jobs = [
                  log @@ U.format_addr conn ^ " addr - count "  ^ (string_of_int count) ^  " " ^ a ^ " port " ^ string_of_int addr.port ;
                  get_connection (formatAddress addr) addr.port ;
                  get_message conn
                ] in
              let state = { state with pending_connections = succ state.pending_connections } in 
              return @@ U.SeqJobFinished (state, jobs)

        | s ->
          let jobs = [
            (* *) log @@ U.format_addr conn ^ " message " ^ s;
            get_message conn
          ] in
          return @@ U.SeqJobFinished (state, jobs)

    | _  ->
      return (U.SeqJobFinished (state, []))




let update state e =
  manage_p2p1 state e
  >>= fun (U.SeqJobFinished (state, jobs1)) ->
    manage_p2p2 state e
  >>= fun (U.SeqJobFinished (state, jobs2)) ->
    return (U.SeqJobFinished (state, jobs1 @ jobs2))



  (* we have to always return U.SeqJobFinished *)

(* VERY IMPORTANT - we can now log sequentially if we want, but we have to be a able to return another job
  uggh... it's gone astray...

  - io completion might be,
    - events
    - seqjobcomplete state [events]

*)


(*
let update (state : U.my_app_state) e =
  match e with
    | U.GotConnection conn ->
		let connections = conn :: state.connections in
	{ state with
		connections = connections;
		jobs = state.jobs @
      [
          log @@ U.format_addr conn ^  " got connection "  ^
            ", connections now " ^ ( string_of_int @@ List.length connections )
          >> U.send_message conn initial_version
          >> log @@ "*** sent our version " ^ U.format_addr conn
          ;
           get_message conn
      ] }

    | U.GotConnectionError msg ->
	  { state with
      jobs = state.jobs @ [  log @@ "connection error " ^ msg ]
		}

    | U.GotMessageError ((conn : U.connection), msg) ->
      (* fd test is physical equality *)
	  { state with
		connections = List.filter (fun (c : U.connection) -> c.fd != conn.fd) state.connections;
		jobs = state.jobs @
       [
        log @@ U.format_addr conn ^ "msg error " ^ msg;
        match Lwt_unix.state conn.fd with
          Opened -> ( Lwt_unix.close conn.fd ) >> return U.Nop
          | _ -> return U.Nop
      ] }

    | U.GotMessage (conn, header, raw_header, payload) ->
      (
      match header.command with

        | "version" ->
		{ state with
             jobs = state.jobs @ [
              log @@ U.format_addr conn ^ " got version message"
              >> U.send_message conn initial_verack
              >> log @@ "*** sent verack " ^ U.format_addr conn
              ;
              get_message conn
            ] }

        | "verack" ->
			{ state with
             jobs = state.jobs @ [
              (* should be 3 separate jobs? *)
              log @@ U.format_addr conn ^ " got verack";
              (* >> send_message conn initial_getaddr *)
              get_message conn
            ] }

        | "addr" ->
            let pos, count = M.decodeVarInt payload 0 in
            (* should take more than the first *)
            let pos, _ = M.decodeInteger32 payload pos in (* timeStamp  *)
            let _, addr = M.decodeAddress payload pos in
            let formatAddress (h : M.ip_address ) =
              let soi = string_of_int in
              let a,b,c,d = h.address  in
              String.concat "." [
              soi a; soi b; soi c; soi d
              ] (* ^ ":" ^ soi h.port *)
            in
            let a = formatAddress addr in
            (* ignore, same addr instances on different ports *)
            let already_have = List.exists (fun (c : U.connection) -> c.addr = a (* && peer.conn.port = addr.port *) ) state.connections
            in
            if already_have || List.length state.connections >= 8 then
				{ state with
                 jobs = state.jobs @ [
                  log @@ U.format_addr conn ^ " addr - already got or ignore "
                    ^ a ^ ":" ^ string_of_int addr.port ;
                  get_message conn
                  ] }
            else
				{ state with
					jobs = state.jobs @
				[
                 log @@ U.format_addr conn ^ " addr - count "  ^ (string_of_int count )
                    ^  " " ^ a ^ " port " ^ string_of_int addr.port ;
                  get_connection (formatAddress addr) addr.port ;
                  get_message conn
                ] }

        | s ->
			{ state with
				jobs = state.jobs @
             [
              (* log @@ U.format_addr conn ^ " message " ^ s ; *)
              get_message conn
              ] }
        )

    | _ -> state
*)

(* initial jobs *)
let create () = [
  (* https://github.com/bitcoin/bitcoin/blob/master/share/seeds/nodes_main.txt *)
(*
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
*)

   (* nc -v  dnsseed.litecointools.com 9333 *)
(*  get_connection     "212.71.235.114"  9333;
  get_connection     "199.217.119.33" 9333;
  get_connection     "46.28.206.65" 9333;
  get_connection     "188.138.125.48" 9333;
  get_connection     "24.160.59.242" 9333;
*)

(* doge  https://github.com/lian/bitcoin-ruby/blob/master/lib/bitcoin.rb
    nc -v seed.dogecoin.com  22556
  *)
(*
  get_connection "162.243.251.36" 22556;
  get_connection "128.250.195.242" 22556;
  get_connection "216.155.138.34" 22556;
  get_connection "128.199.78.238" 22556;
*)
]

