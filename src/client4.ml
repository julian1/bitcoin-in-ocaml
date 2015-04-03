

open Lwt (* for >>= *)

type acc_type =
{
  (* what is it that we want to record - the uxto set  
    or changes... 
  *)

  count : int;
}



Lwt_main.run (

  let write_stdout = Lwt_io.write_line Lwt_io.stdout in

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
  (* to scan the messages stored in file 
    this thing is effectively a fold. 
    don't we want to be able to compute something...
    perhaps monadically...
  *)
  let rec fold fd f (acc : acc_type ) =
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
  let f payload ( acc : acc_type ) = 
    let hash = String.sub payload 0 80 |> Message.sha256d |> Message.strrev in
    (* decode block header *)
    let pos, _ = Message.decodeBlock payload 0 in
    let pos, tx_count = Message.decodeVarInt payload pos in 
    (* decode txs *)
    let first = pos in
    let _, txs = Message.decodeNItems payload pos Message.decodeTx tx_count in
    (* extract tx start/lengths *)
    let lens = List.map (fun (tx : Message.tx) -> tx.bytes_length) txs in 
    let starts = List.fold_left (fun acc len -> List.hd acc + len::acc) [first] lens |> List.tl |> List.rev in 
    let zipped = Core.Core_list.zip_exn starts lens in
    (* tx hashes *)
    let hashes = List.map (fun (start,len) -> 
      String.sub payload start len 
      |> Message.sha256d 
      |> Message.strrev ) zipped 
    in
    (* associate hash with tx *)
    let txs = Core.Core_list.zip_exn hashes txs in

(*      List.map (fun (hash,tx) -> write_stdout hash) txs in 
*)

    (* - want to associate the hash with the tx - so can look up 
        and invalidate... 
    *)
    (
    if (mod) acc.count 1000 = 0 then
      write_stdout @@ 
        "*****\n"
        ^ string_of_int acc.count 
        ^ " " ^ Message.hex_of_string hash 
        ^ " " ^ string_of_int tx_count  
      else
        return () 
    )      
    >> return { acc with count = (acc.count + 1) }
  in

  Lwt_unix.openfile "blocks.dat"  [O_RDONLY] 0 
  >>= fun fd -> 
    match Lwt_unix.state fd with 
      Opened -> 
        write_stdout "scanning blocks..." 
        >> fold fd f { count = 0 ; }
        >>= fun acc ->   
          write_stdout ("result " ^ (string_of_int acc.count ))
        >> Lwt_unix.close fd
      | _ -> 
        write_stdout "couldn't open file"
)

