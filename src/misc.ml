
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
   | GotMessageError of connection * string (* change name MessageError - because *)
   | Nop


let log s = write_stdout s >> return Nop 

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


  (* really should be able to hide this *)
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



