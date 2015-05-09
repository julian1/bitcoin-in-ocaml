
(* there's a bunch of string manipulation functions in message.ml that
are not encode or decode that should probably go here *)

let (>>=) = Lwt.(>>=)
let return = Lwt.return

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


(* useful for debugging *)
let string_of_bytes s =
  explode s |> List.map (fun ch -> ch |> Char.code |> string_of_int ) |> String.concat " "



let read_bytes fd len =
  let block = Bytes .create len in
  Lwt_unix.read fd block 0 len >>=
  fun ret ->
    (* Lwt_io.write_line Lwt_io.stdout @@ "read bytes - "  ^ string_of_int ret >>  *)
  return (
    if ret = len then Some ( Bytes.to_string block )
    else None
    )

let write_stdout = Lwt_io.write_line Lwt_io.stdout

let pad s length =
    let n = length - String.length s + 1 in
    if n > 0 then
      s ^ String.make n ' '
    else
      s




type connection =
{
  (* addr : ip_address;
    when checking if connecting to same node, should check ip not dns name
    *)
	(* we also ought to be able to get the addr and port from the fd *)
  addr : string ;
  port : int;
  fd :  Lwt_unix.file_descr ;
  (* get rid of this,  sockets don't need buffers *)
  ic : Lwt_io.input Lwt_io.channel ;
  oc : Lwt_io.output Lwt_io.channel ;
}



let format_addr conn =
  let s = conn.addr ^ ":" ^ string_of_int conn.port in
  pad s 18


type my_event =
   | GotConnection of connection
   | GotConnectionError of string
   | GotMessage of connection * Message.header * string * string
   | GotMessageError of connection * string
   | Nop


type jobs_type =  my_event Lwt.t list 




(* module SS = Map.Make(struct type t = string let compare = compare end) *)
module SS = Map.Make( String ) 

module SSS = Set.Make(String);;

type my_head =
{
  (*  hash : string; *)
    previous : string;  (* could be a list pointer at my_head *)
    height : int;   (* if known? *)
   (*  difficulty : int ; *) (* aggregated *)
    (* bool have *)
}


(* shared between different p2p, chain functions etc 
	think it's going to be a problem .... with recursive stuff...

	Ok, doesn't work because the chain module refers to misc 
	and misc module refers to chain.

	- So let's move the Chain structure in here as well for the moment...
	- hmmm 
----------------------

	No we don't drag the chain in here we just expand the type ...
*)

type ggg = {
  fd : Lwt_unix.file_descr ;
  t : float ;
}


type my_app_state =
{
  jobs :  jobs_type  ;

  (*
    - we have jobs that are async, (network management) could complete at any time (eg. new connection)
    - we also have io jobs that kind of need to be synchronized

      block -> validate (io - eg. check uxto) -> add to heads -> write to blocks.dat -> update indexes

    - these can potentially involve race conditions... eg. two blocks arriving adjacent in time
    - we could have a queue of pending actions... where lwt would be () type...
    - the main client would pick a job, and put in jobs queue. then when it's run it would
      take the next job if there was one... 

    - yes - we need to have multiple connection attempts in parallel. any one could complete 
    -----------
    - we either use a queue and messages... 

    - else the entire sequence has to take a mutex and be allowed to run to completion, using joins ... 
      which is kind of easier...
     
    --- so we receive a block it's put on a queue.

    - then the block processing sequence takes it and a mutex, and runs everything in sequence program...
      eg. 
		>>= verify block, against heads hash, uxtos etc... 
		>>= write_block 
		>>= update heads 
		>>= determine pow
		>>= update indexes 

		BUT. importantly if it accesses the heads structure - nothing else should be reading it ... 

		problem of the heads...
			- can fix by splitting sequence two and coordinating around the heads update...

		>>= is the natural way to sequence. compared with breaking up messages.:w 
			and we can use mutex which is the same as a queue.
		-----
		- issue is the chain management has to read heads to handle chain download...
  *)

  connections : connection list ;

  (****** updated by chain ******)
	(* tree structure  - change name tree *)
  heads : my_head SS.t ;

  (* set when inv request made to peer *)
  block_inv_pending  : (Lwt_unix.file_descr * float ) option ;

  (* blocks requested - peer, time, solicited *)
  blocks_on_request : (Lwt_unix.file_descr * float * bool ) SS.t ;

  (*  last_block_received_time : (Lwt_unix.file_descr * float) list ; *)
  last_block_received_time : ggg list ;

  blocks_fd_m : Lwt_mutex.t ;

  blocks_fd : Lwt_unix.file_descr ;
 
  (* db : LevelDB.db ; *)


  (* chain :  Chain.t;  *)
}





(*
  - roads, on physical earth
  - law
  - currency mint
  - church
*)
(*
  HERE
  - so we'd pass chainstate jobs and connections and event to the chainstate module.
  - and likewise we'd pass p2p, jobs and connections to the p2p module.

  - only only bring stuff together at the end. 

  - each function is responsible for updating some of the state in response
  to an event.

  - we may want f tuple e, because it's like a fold. 
  - only the top level has all the chunks in one place.

  - and we can move the my_app_state into it's own thing as well...
  ------
  - the issue is that the module m hides module m implementation / state .
*)

(* this really shouldn't be here - place it in app_state? *)

(*  bitcoin magic_head: "\xF9\xBE\xB4\xD9",
  testnet magic_head: "\xFA\xBF\xB5\xDA",
  litecoin magic_head: "\xfb\xc0\xb6\xdb",

(* let m = 0xdbb6c0fb   litecoin *)
*)

let magic = 0xd9b4bef9  (* bitcoin *)


let send_message conn s =
    let oc = conn.oc in
    Lwt_io.write oc s >> return Nop  (* message sent *)

module M = Message

(* 
  - move this to message.ml, 
  issue is that it refers to the magic value ...
  so we'd have to put that in message also...
  - or else just parametize message 
  - we'd have to pass this var around everywhere... 
*) 
let encodeMessage command payload = 
  let header = M.encodeHeader {
    magic = magic ;
    command = command ;
    length = M.strlen payload;
    checksum = M.checksum payload;
  } in
  header ^ payload


let encodeSimpleMessage command = 
  M.encodeHeader {
    magic = magic ;
    command = command;
    length = 0;
    (* clients seem to use e2e0f65d - hash of first part of header? *)
    checksum = 0;
  } 
 

