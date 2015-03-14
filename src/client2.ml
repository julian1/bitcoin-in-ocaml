
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

let initial_getblocks =
  let genesis = "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f" in
  let payload = 
    encodeInteger32 1 
    ^ encodeVarInt 1 
    ^ encodeHash32 genesis 
    ^ encodeInteger32 0
	in
  let header = encodeHeader {
    magic = m ;
    command = "getblocks";
    length = strlen payload;
    checksum = checksum payload;
  } in
	header ^ payload
	





let handleMessage header payload outchan =
  (* we kind of want to be able to write to stdout here 
    and return a value...
    - we may want to do async database actions here. so keep the io
  *)
  match header.command with
  | "version" -> 
    let _, version = decodeVersion payload 0 in
    Lwt_io.write_line Lwt_io.stdout ("* whoot got version\n" ^ formatVersion version)
    >>= fun _ -> Lwt_io.write_line Lwt_io.stdout "* sending verack"
    >>= fun _ -> Lwt_io.write outchan initial_verack

  | "verack" -> 
    Lwt_io.write_line Lwt_io.stdout ("* got verack" )
	(* ok, this is the point to send our real request *)

    >>= fun _ -> Lwt_io.write outchan initial_getblocks 





  | "inv" -> 
    let _, inv = decodeInv payload 0 in
    Lwt_io.write_line Lwt_io.stdout ("* whoot got inv" ^ formatInv inv )
    (* request inventory item 
		Ok, we don't want to request all inventory items 
	*)
	(*	
    >>=  fun _ -> 
      let header = encodeHeader {
        magic = m ;
        command = "getdata";
        length = strlen payload;
        checksum = checksum payload;
      } in 
      Lwt_io.write outchan (header ^ payload )
	*)

  | "tx" -> ( 
      let _, tx = decodeTx payload 0 in 
      Lwt_io.write_line Lwt_io.stdout ( "* got tx!!!\n"  )
    )


  | "block" -> 
        (* let hash = Message.sha256d payload |> strrev |> hex_of_string in *)
	      let hash = (Message.strsub payload 0 80 |> Message.sha256d  |> Message.strrev |> Message.hex_of_string) in
        Lwt_io.write_line Lwt_io.stdout ( "* got block " ^ hash ^ "\n" )
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



(* read exactly n bytes from channel, returning a string 
	- change name to readn or something? *) 
let readChannel inchan length   =
  let buf = Bytes.create length in 
  Lwt_io.read_into_exactly inchan buf 0 length 
  >>= fun _ -> 
    return @@ Bytes.to_string buf


let mainLoop inchan outchan =
  let rec loop () =
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
    >>= fun s -> handleMessage header s outchan
    (* repeat *)
    >>= fun _ -> loop ()
  in
    loop()


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



