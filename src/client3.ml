
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



type myvar =
   | GotConnection of Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel
   | GotMessage of Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel * header * string
   | GotError of string
   | Nop


(* read exactly n bytes from channel, returning a string
	- change name to readn or something? *)
let readChannel inchan length   =
  let buf = Bytes.create length in
  Lwt_io.read_into_exactly inchan buf 0 length
  >>= fun _ ->
    return @@ Bytes.to_string buf



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
  Lwt_unix.gethostbyname host
  >>= fun entry ->
    if Array.length entry.Unix.h_addr_list = 0 then
      return @@ GotError "could not resolve hostname"
    else
      Lwt.catch
        (fun  () ->
         let a = (Unix.ADDR_INET (entry.Unix.h_addr_list.(0) , port)) in
        let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        let (inchan : 'mode Lwt_io.channel )= Lwt_io.of_fd ~mode:Lwt_io.input fd in
        let outchan = Lwt_io.of_fd ~mode:Lwt_io.output fd in
        Lwt_unix.connect fd a
        >> return @@ GotConnection (inchan, outchan)
        )
        (fun exn ->
          let s = Printexc.to_string exn in
          return @@ GotError s
        )



let readMessage ic oc =
    readChannel ic 24
    (* log *)
  (*   >>= fun s ->
      let _, header = decodeHeader s 0 in
        let _ = Lwt_io.write_line Lwt_io.stdout ("----\n" ^ Message.hex_of_string s ^ "\n" ^ Message.formatHeader header ^ "\n")  in
		return s
	*)
    (* read payload *)
    >>= fun s ->
      let _, header = decodeHeader s 0 in
      readChannel ic header.length
    >>= fun p ->
      return @@ GotMessage ( ic, oc, header, p)

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

let run () =

  Lwt_main.run (
    (*
      ok hang on.
        - it would be better to take a list of arrays and return a list
		- more symetrical, and allows Nop to be []
		- but we actually want state to be updated... 

		- f s e -> s
    *)

    (*
    - lst = jobs, tasks, threads, fibres
    - within a task we can sequence as many sub tasks using >>= as we like
    - we only have to thread stuff through this main function, when the app state changes
       and we have to synchronize, 
    - app state is effectively a fold over the network events...
    *)
    let rec loop lst =

      Lwt.nchoose_split lst
      >>= fun (complete, incomplete) ->
        Lwt_io.write_line Lwt_io.stdout @@
        "complete length " ^ (string_of_int @@ List.length complete )
        ^ " incomplete length " ^ (string_of_int @@ List.length incomplete)
      >>
        let f e =
        match e with
          | GotConnection (ic, oc) ->
            Lwt_io.write_line Lwt_io.stdout "whoot got connection "
            >> Lwt_io.write oc initial_version
            >> readMessage ic oc

          | GotError msg ->
            (Lwt_io.write_line Lwt_io.stdout msg
            >> return Nop
            )

          | GotMessage (ic, oc, header, payload) -> (
            match header.command with
              | "version" ->
              Lwt_io.write_line Lwt_io.stdout "version message"
              >> Lwt_io.write oc initial_verack
              >> readMessage ic oc

              | "inv" ->
              let _, inv = decodeInv payload 0 in
              Lwt_io.write_line Lwt_io.stdout @@ "* whoot got inv" (* ^ formatInv inv *)
              >> readMessage ic oc

              | s ->
              Lwt_io.write_line Lwt_io.stdout @@ "message " ^ s
              >> readMessage ic oc
          )

          | Nop -> return Nop

      in
      (* let complete = List.filter (fun x -> match x with | Nop -> false | _ -> true ) complete in *)
      let complete = List.filter (fun x -> x != Nop  ) complete in
      let continuations = List.map f complete in
      (* should filter Nop *)
      loop (continuations @ incomplete)


    in

     let lst = [
        getConnection "198.52.212.235"  8333;
    (*    getConnection "198.52.212.235"  8333 *)
    ] in

    loop lst
  )


let () = run ()


