 

open Lwt (* for >>= *)

Lwt_main.run (

  let read_bytes fd len =
    let block = Bytes .create len in
    Lwt_unix.read fd block 0 len >>= 
    fun ret ->
      (* Lwt_io.write_line Lwt_io.stdout @@ "read bytes - "  ^ string_of_int ret >>  *)
    return (
      if ret = len then Some ( Bytes.to_string block )
      else None 
      )
  in
(*
  let advance fd len =
      Lwt_unix.lseek fd len SEEK_CUR 
      >>= fun r -> 
      (* Lwt_io.write_line Lwt_io.stdout @@ "seek result " ^ string_of_int r  *)
      return ()
  in
*)
  (* to scan the messages stored in file 
    this thing is effectively a fold. 
    don't we want to be able to compute something...
    perhaps monadically...
  *)
  let rec fold fd f acc =
    read_bytes fd 24
    >>= fun x -> match x with 
      | None -> return acc 
      | Some s -> ( 
        let _, header = Message.decodeHeader s 0 in
        (* Lwt_io.write_line Lwt_io.stdout @@ header.command ^ " " ^ string_of_int header.length >> *) 
        read_bytes fd header.length 
        >>= fun u -> match u with 
          | None -> return acc 
          | Some payload -> 
              f payload acc 
              >>= fun acc -> (* advance fd (header.length - 80 )
              >> *) fold fd f acc 
        )
  in 

  let f payload acc = 
    let hash = payload |> Message.sha256d |> Message.strrev in
    let pos, block_header = Message.decodeBlock payload 0 in
    let pos, tx_count = Message.decodeVarInt payload pos in 

    Lwt_io.write_line Lwt_io.stdout @@ 
      "*****\n"
      ^ string_of_int acc 
      ^ " " ^ Message.hex_of_string hash 
      ^ " " ^ string_of_int tx_count  

    (*Message.decodeTx payload pos in  
  
      we need to extract the txid...
      and see if we can start linking them.
    *)

    >> let pos, txs = Message.decodeNItems payload pos Message.decodeTx tx_count in

    let x = List.map 
      (fun tx -> Lwt_io.write_line Lwt_io.stdout @@ Message.formatTx tx) txs in

    Lwt.join x

    >> return (acc + 1)
  in

  Lwt_unix.openfile "blocks.dat"  [O_RDONLY] 0 
  >>= fun fd -> 
    match Lwt_unix.state fd with 
      Opened -> 
        Lwt_io.write_line Lwt_io.stdout "scanning blocks..." 
        >> fold fd f 0 
        >>= fun acc ->   
          Lwt_io.write_line Lwt_io.stdout ("result " ^ (string_of_int acc ))
        >> Lwt_unix.close fd
)

