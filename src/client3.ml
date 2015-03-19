
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


let initial_getblocks starting_hash =
  (* we can only request one at a time 
    - the list are the options, and server returns a sequence
    from the first valid block in our list
  *)
  let payload =
    encodeInteger32 1  (* version *)
    ^ encodeVarInt 1
    ^ encodeHash32 starting_hash
    ^ zeros 32		(* block to stop - we don't know should be 32 bytes *)
	in
  let header = encodeHeader {
    magic = m ;
    command = "getblocks";
    length = strlen payload;
    checksum = checksum payload;
  } in
	header ^ payload



module SS = Map.Make(struct type t = string let compare = compare end)



type myblock = 
{
    hash : string;
    previous : string;  (* could be a list pointer at myblock *) 
    height : int;   (* if known? *)
    difficulty : int ; (* aggregated *)
    (* bool requested *)
    (* bool have *)
}


type my_app_state =
{
  count : int;
  (* or blocks on request? *)
  heads : myblock SS.t;	
  time_of_last_received_block : float;
  handshake_complete : bool ;
}


let handleMessage (state: my_app_state) header payload outchan =

  let now = Unix.time () in  (* state, time is seconds, gettimeofday more precision *)

  let with_return_state state =

    let update_count state = 
      return { state with count = state.count + 1;} 
    in  
 
    let request_more_blocks state = 
(*      if state.handshake_complete == true 
        && now > state.time_of_last_received_block +. 10. then 

        (* we need to avoid sending request too often than once *)
        let state = { state with time_of_last_received_block = now } in 
        let hash,block = SS.min_binding state.heads in
        Lwt_io.write_line Lwt_io.stdout ("****  requesting more blocks - from " ^ hex_of_string hash )
        >> Lwt_io.write outchan (initial_getblocks hash) 
        >> return state
      else
        return state
*)
      return state
    in

    let request_addrs state = 
      return state

    in
      update_count state >>= request_more_blocks >>= request_addrs

  in

  match header.command with
  | "version" ->
    let _, version = decodeVersion payload 0 in
    Lwt_io.write_line Lwt_io.stdout ("* whoot got version\n" ^ formatVersion version)
    >> Lwt_io.write_line Lwt_io.stdout "* sending verack"
    >> Lwt_io.write outchan initial_verack
    >> with_return_state state

  | "verack" ->
    Lwt_io.write_line Lwt_io.stdout ("* got verack - handshake complete"  )
    >> let state = { state with handshake_complete = true; } in
    with_return_state state


  | "inv" ->
    let _, inv = decodeInv payload 0 in
    Lwt_io.write_line Lwt_io.stdout ("* whoot got inv" ^ formatInv inv ^ " state " ^ string_of_int state.count  )
    (* request inventory item
		  Ok, we don't want to request all inventory items
	  *)
    (* TODO we calculate block_hashes but aren't explicitly requesting them *)
    >> let block_hashes = inv
        |> List.filter (fun (inv_type,hash) -> inv_type == 2)
        |> List.map (fun (_,hash)->hash)
      in
      (* request data - we need to encode this .... *)
      let header = encodeHeader {
        magic = m ;
        command = "getdata";
        length = strlen payload;
        checksum = checksum payload;
      } in
      Lwt_io.write outchan (header ^ payload )
      >> with_return_state state

  | "addr" -> (
      let _, count = decodeVarInt payload 0 in
      Lwt_io.write_line Lwt_io.stdout ( "* got addr - count " ^ string_of_int count ^ "\n" )
      >> with_return_state state
    )

  | "tx" -> (
      let _, tx = decodeTx payload 0 in
      Lwt_io.write_line Lwt_io.stdout ( "* got tx!"  )
      >> with_return_state state
    )

  | "block" ->
      (* so we need to decode the header - to look at the previous  *)
      let hash = (Message.strsub payload 0 80 |> Message.sha256d |> Message.strrev ) in
      let _, header = decodeBlock payload 0 in 
      Lwt_io.write_line Lwt_io.stdout ( "* got block " ^ ( Message.hex_of_string hash) 
        ^ " previous " ^  Message.hex_of_string header.previous ^ "\n" )
      >> 
      (* does this block point at a head - if so we update the head! *)
      if SS.mem header.previous state.heads then 
        let old = SS.find header.previous state.heads in 
        let new_ = { hash = hash; previous = header.previous; height = old.height + 1 ; difficulty = 123 } in 
        let state = { 
          state with 
          time_of_last_received_block = now;
          heads = 
            state.heads
            |> SS.remove header.previous 
            |> SS.add hash new_ ;
        } 
        in 
          (* ugghhh and we will have to request the new block *)
          Lwt_io.write_line Lwt_io.stdout ( "* got block and updated head " 
            ^ "\n hash " ^   Message.hex_of_string new_.hash
            ^ "\n previous " ^   Message.hex_of_string new_.previous
            ^ "\n height " ^ string_of_int new_.height 
            ^ "\n size " ^ string_of_int (strlen payload) 
            ^ "\n" )
        >> with_return_state state
      else
        Lwt_io.write_line Lwt_io.stdout ( "* got block (ignored) "  ^ string_of_int (strlen payload) ^ "\n" )
        (* should update time_of_last_received_block ? *)
        >> with_return_state state


(*
      >>= fun _ ->
        let filename =  "./blocks/" ^ hash in
        Lwt_unix.openfile filename [O_WRONLY;O_CREAT;O_TRUNC] 0o644
      >>= fun fd ->
        Lwt_io.write_line Lwt_io.stdout ( "* opening file '" ^ filename ^ "'" )
      >>= fun _ ->
        Lwt_unix.write fd payload 0 header.length
      >>= fun bytes_written ->
        Lwt_unix.close fd
      >>= fun _ ->
        let result = Printf.sprintf "* %d of %d written, closing"  bytes_written header.length in
        Lwt_io.write_line Lwt_io.stdout result
*)

  | _ ->
    Lwt_io.write_line Lwt_io.stdout ("* unknown '" ^ header.command ^ "' " ^ " length " ^ string_of_int header.length )
    >> with_return_state state




(* read exactly n bytes from channel, returning a string
	- change name to readn or something? *)
let readChannel inchan length   =
  let buf = Bytes.create length in
  Lwt_io.read_into_exactly inchan buf 0 length
  >>= fun _ ->
    return @@ Bytes.to_string buf


let mainLoop inchan outchan =
  let rec loop state =
    (* read header *)
    readChannel inchan 24
    (* log *)
  (*   >>= fun s ->
      let _, header = decodeHeader s 0 in
        let _ = Lwt_io.write_line Lwt_io.stdout ("----\n" ^ Message.hex_of_string s ^ "\n" ^ Message.formatHeader header ^ "\n")  in
		return s
	*)

    (* read payload *)
    >>= fun s ->
      let _, header = decodeHeader s 0 in
      readChannel inchan header.length
    (* handle  *)
    >>= fun s -> handleMessage state header s outchan
    (* repeat *)
    >>= fun state  -> loop state
  in
  let starting_state = {
    count = 1;
    heads = ( 
      (* as an exception - we add genesis even though it's not been downloaded it yet *)
      let genesis = string_of_hex "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f" in 
      let j = SS.empty in
      let j = SS.add genesis { 
        hash = genesis; 
        previous = ""; 
        height = 0; 
        difficulty = 123 
      } j in
      j );
    time_of_last_received_block = Unix.time (); (* 0.; *)
    handshake_complete = false;


  }
  in
    loop starting_state


let addr ~host ~port =
  lwt entry = Lwt_unix.gethostbyname host in
  if Array.length entry.Unix.h_addr_list = 0 then begin
    failwith (Printf.sprintf "no address found for host %S\n" host)
  end;
  return (Unix.ADDR_INET (entry.Unix.h_addr_list.(0) , port))


let run () =
  Lwt_main.run (
 (*    addr ~host: "50.68.44.128" ~port: 8333 *)  (* was good, no more *)
    (*    149.210.187.10  *)
      (* addr ~host: "173.69.49.106" ~port: 8333 *) (* no good *)
      addr ~host: "198.52.212.235" ~port: 8333   (* good, not anymore, good *)


     (* addr ~host: "dnsseed.litecointools.com" ~port: 9333 *) (* litecoin *)


    >>= fun ip -> Lwt_io.write_line Lwt_io.stdout "decoded address "
    (* connect *)
    >>= fun () -> let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let inchan = Lwt_io.of_fd ~mode:Lwt_io.input fd in
      let outchan = Lwt_io.of_fd ~mode:Lwt_io.output fd in
      Lwt_unix.connect fd ip
    (* send version *)
    >>= fun _ -> Lwt_io.write outchan initial_version
    >>= fun _ -> Lwt_io.write_line Lwt_io.stdout "sending version"
    (* enter main loop *)
    >>= fun _ -> mainLoop inchan outchan
    (* return () *)
    (*  >>= (fun () -> Lwt_unix.close fd)  *)
  )


let () = run ()



