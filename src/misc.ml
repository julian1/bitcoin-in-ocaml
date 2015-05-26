
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

	(* hash, height, raw_header, payload *)
   | GotBlock of string * int * string * string 

	| SeqJobFinished  (* SeqJobOK , SeqJobFailed? *)
   | Nop


type jobs_type =  my_event Lwt.t list 



(* module SS = Map.Make(struct type t = string let compare = compare end) *)
module SS = Map.Make( String ) 

module SSS = Set.Make(String);;

type my_head =
{
  (* downloaded block, not necessarirly saved/and processed *)
  (*  hash : string; *)
    previous : string;  (* could be a list pointer at my_head *)
    height : int;   (* if known? *)
   (*  difficulty : int ; *) (* aggregated *)
}


(* revert this back to a tuple instead *)
type ggg = {
  fd : Lwt_unix.file_descr ;
  t : float ;
}


type my_app_state =
{
  jobs :  jobs_type  ;

  connections : connection list ;

  (* tree structure  - change name tree *)
  heads : my_head SS.t ;

  (* set when inv request made to peer *)
  block_inv_pending  : (Lwt_unix.file_descr * float ) option ;

  (* blocks requested - peer, time, solicited *)
  blocks_on_request : (Lwt_unix.file_descr * float * bool ) SS.t ;

  (*  last_block_received_time : (Lwt_unix.file_descr * float) list ; *)
  last_block_received_time : ggg list ;


  seq_jobs_pending : (unit -> my_event Lwt.t ) Myqueue.t;	

  seq_job_running : bool ;

  blocks_fd : Lwt_unix.file_descr ;
 
  db : LevelDB.db ; 
}



(* the head structure shouldn't be here - place it in app_state? *)

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
 

