

open Lwt (* for >>= *)

module M = Message
module L = List


module SSS = Set.Make(String);; 



type acc_type =
{
  (* what is it that we want to record - the uxto set  
    or changes... 
  *)

  count : int;
	
  utxos : SSS.t ;	
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
        let _, header = M.decodeHeader s 0 in
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
	(* IMPORTANT - all the code that isn't IO should be factored out *)
    let block_hash = M.strsub payload 0 80 |> M.sha256d |> M.strrev in
    (* decode block header *)
    let pos, _ = M.decodeBlock payload 0 in
    let pos, tx_count = M.decodeVarInt payload pos in 
    (* decode txs *)
    let first = pos in
    let _, txs = M.decodeNItems payload pos M.decodeTx tx_count in
    (* extract tx start/lengths *)
    let lens = L.map (fun (tx : M.tx) -> tx.bytes_length) txs in 
    let starts = L.fold_left (fun acc len -> L.hd acc + len::acc) [first] lens |> L.tl |> L.rev in 
    let zipped = Core.Core_list.zip_exn starts lens in
    (* tx hashes *)
    let hashes = L.map (fun (start,len) -> 
      M.strsub payload start len 
      |> M.sha256d 
      |> M.strrev ) zipped 
    in
    (* associate hash with tx *)
    let txs = Core.Core_list.zip_exn hashes txs in

(*
  - ahh remember we specify an index with the output we want... 
  - rather than pass utxos through here, 
    why not pass the accumulator? 
    - then we can record lots of stats...
  -- can also add logging jobs...

    OK. input unlocks an output
    but we have indexes 

    tx has a bunch of outputs.  

    an input can spend any of the outputs. but we need to specify... 

    tx outputs... meaning tx and index 

    when we add, we'll have to add for each output...
    
    so this means a set using output and hash...
*)
    let coinbase = M.zeros 32 in

    (* so replace the previous - but with what ? the tx out?  *)
    let utxos = L.fold_left (fun utxos (hash,( tx : M.tx) ) -> 
      L.fold_left (fun utxos (input : M.tx_in) -> 

        if input.previous = coinbase then  
          (* coinbase*) 
          SSS.add hash utxos
        else
          if SSS.mem input.previous utxos then   
             utxos  
(*            |> SSS.remove input.previous  *)
            |> SSS.add hash
          else
            (* shouldn't happen *)
            raise (Failure 
                (
                "block " ^ string_of_int acc.count
                ^ " hash " ^ M.hex_of_string hash 
                ^ " previous " ^ M.hex_of_string input.previous) )
          ) utxos tx.inputs
    ) acc.utxos txs in 

  let _ = 
  if (mod) acc.count 1000 = 0 then
   
    write_stdout @@ (string_of_int acc.count) ^ " " ^ (M.hex_of_string block_hash) 
    >> write_stdout @@ "utxo count " ^ string_of_int @@ SSS.cardinal utxos 
  else
    return ()
  in
(* 
	let _ =  Lwt.join @@ L.map ( fun (hash,tx) -> 
		write_stdout @@ " " ^ M.hex_of_string hash ^ M.formatTx tx) txs in 
*)

    (* - want to associate the hash with the tx - so can look up 
        and invalidate... 
    *)
(*    (
    if (mod) acc.count 1000 = 0 then
      write_stdout @@ 
        "*****\n"
        ^ string_of_int acc.count 
        ^ " " ^ M.hex_of_string hash 
        ^ " " ^ string_of_int tx_count  
      else
        return () 
    ) *)      
    return { acc with count = (acc.count + 1); utxos = utxos; }
  in

  Lwt_unix.openfile "blocks.dat"  [O_RDONLY] 0 
  >>= fun fd -> 
    match Lwt_unix.state fd with 
      Opened -> 
        write_stdout "scanning blocks..." 
        >> fold fd f { count = 0 ; utxos = SSS.empty; }
        >>= fun acc ->   
          write_stdout ("result " ^ (string_of_int acc.count  ))
        >> Lwt_unix.close fd
      | _ -> 
        write_stdout "couldn't open file"
)

