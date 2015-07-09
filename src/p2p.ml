
let (>>=) = Lwt.(>>=)
let return = Lwt.return

module M = Message
module U = Misc


let log s = U.write_stdout s >> return U.Nop


(* initial version message to send *)
let initial_version =
  let payload = M.encodeVersion {
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
  U.encodeMessage "version" payload

(* verack response to send *)
let initial_verack =
  U.encodeSimpleMessage "verack"

 
let initial_getaddr =
  U.encodeSimpleMessage "getaddr"



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
        )
        (fun exn ->
          (* must close *)
          let s = Printexc.to_string exn in
          Lwt_unix.close fd
          >> return @@ U.GotConnectionError s
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

(* let update connections e = *)


let update state e = 
  match e with
    | U.GotConnection conn ->
    log "whoot got connection"
    | _  -> return ()

  >>
  return (Misc.SeqJobFinished state)

(* ok, this is interesting because we can actually log sequentially *)


(*
let update (state : Misc.my_app_state) e =
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
            let already_got = List.exists (fun (c : U.connection) -> c.addr = a (* && peer.conn.port = addr.port *) ) state.connections
            in
            if already_got || List.length state.connections >= 8 then
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
] 



