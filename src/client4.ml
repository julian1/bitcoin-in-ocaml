
(*
  - instead of building index of just utxos we may as well just have an index 
  of all txo's and
  then just add bits as required, to mark whether it's spent or not.
  - and even for unconfirmed tx as well, could go in same.

  ------
  
  Think that we need a module interface, then we can use leveldb or sql behind it... 
    add      tx  lseek 
    remove   tx
    check    tx

  to do this we need to change code to use io 
*)

open Lwt (* for >>= *)

module M = Message
module L = List
module CL = Core.Core_list

(*

module SSS = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = string * int
  end 
)

*)
(*
let detach f = 
  Lwt_preemptive.detach 
    (fun () -> f ) () 
*)


(*
module SSS = Set.Make( String );; 
*)

(*
type acc_type =
{
  (* what is it that we want to record - the uxto set  
    or changes... 
  *)

  count : int;
	
  utxos : LevelDB.db ;	
}
*)

let decodeTXXX payload =
    (* move to Message ? *)
    let pos = 80 in
    let pos, tx_count = M.decodeVarInt payload pos in 
    (* decode txs *)
    let first = pos in
    let _, txs = M.decodeNItems payload pos M.decodeTx tx_count in
    (* extract tx start/lengths *)
    let lens = L.map (fun (tx : M.tx) -> tx.bytes_length) txs in 
    let starts = L.fold_left (fun acc len -> L.hd acc + len::acc) [first] lens |> L.tl |> L.rev in 
    let zipped = CL.zip_exn starts lens in
    (* tx hashes *)
    let hashes = L.map (fun (start,len) -> 
      M.strsub payload start len 
      |> M.sha256d 
      |> M.strrev ) zipped 
    in
    (* associate hash with tx *)
    let txs = CL.zip_exn hashes txs in
    txs


let write_stdout = Lwt_io.write_line Lwt_io.stdout 

(*
  so we need to change utxos to be a db structure...
  - add_block       (add and replace txos)
  - remove_block    (remove txos) 

  but how do we deal with addresses?
*)


let update_for_tx db hash (tx : M.tx)  = 
  let coinbase = M.zeros 32 
  in
(*  let add_outputs outputs hash utxos  = 
    CL.foldi outputs ~f:(fun i utxos output -> 
      SSS.add (hash, i) utxos
    ) ~init: utxos 
  in
*)

  let f (input : M.tx_in) = 

    Lwt.join( 
      L.mapi (fun i output -> 
        Db.put db (M.encodeHash32 hash ^ M.encodeInteger32 i) "u" ) tx.outputs 
    )
    >>
    if input.previous = coinbase then
      (* write_stdout "coinbase" *)
      return ()
    else
      (* delete ?? - actually we don't want to delete, we just want to update to show it's spent 
        should actually verify it exists...
      *) 
      let key = (M.encodeHash32 input.previous ^ M.encodeInteger32 input.index) in
      Db.get db key 
      >>= (fun result ->	
          match result with 
            Some s -> return () (* write_stdout ("found " ^ s)  *)
            | None -> raise (Failure "no found") (* write_stdout "not found "  *)
        ) 
      >>
      Db.put db key "s" 

(*
  ok, i think we want to be able to look at the tx records,
*)

(*
    utxos >>= (fun utxos -> 
    if input.previous = coinbase then  
      write_stdout "here"  
      >> return ( add_outputs tx.outputs hash utxos ) 
    else if SSS.mem (input.previous, input.index) utxos then   
      write_stdout "here2"  
       >> return (
       utxos  
      |> SSS.remove (input.previous, input.index)
      |> add_outputs tx.outputs  hash
      )
    else
      (* referencing a tx that doesn't exist *)
      raise (Failure (
        " hash " ^ M.hex_of_string hash 
        ^ " previous " ^ M.hex_of_string input.previous
        ^ " previous index " ^ string_of_int input.index 
        ) )
    )
*)
  in
  (* fold over inputs *) 

	Lwt.join( 
  ( L.map f tx.inputs )
	)



let run () = 

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
  (* to scan the messages stored in file 
    this thing is effectively a fold. 
    don't we want to be able to compute something...
    perhaps monadically...

    change name do_block read_block... loop_blocks

    db is not used here. so should be bound into f as an initial condition... 
    call it fold_blocks ? 
  *)
  let rec loop_blocks fd f db count =
    read_bytes fd 24
    >>= fun x -> match x with 
      | None -> return ()
      | Some s -> ( 
        let _, header = M.decodeHeader s 0 in
        (* Lwt_io.write_line Lwt_io.stdout @@ header.command ^ " " ^ string_of_int header.length >> *) 
        read_bytes fd header.length 
        >>= fun u -> match u with 
          | None -> return ()
          | Some payload -> 
              f payload db count 
              >>= fun acc -> (* advance fd (header.length - 80 )
              >> *) loop_blocks fd f db (count + 1)
        )
  in 
  let f payload db count = 
    let block_hash = M.strsub payload 0 80 |> M.sha256d |> M.strrev in
    let txs = decodeTXXX payload in
    Lwt.join (
      (* loop_blocks over txs *) 
      L.map (fun (hash, tx  ) -> 
        update_for_tx db hash tx   
      )  txs  
    )
    >> if count mod 1000 = 0 then 
      write_stdout @@ string_of_int count ^ " " ^ M.hex_of_string block_hash
    else
      return ()

(*    >>= (fun utxos -> 
      write_stdout @@  (M.hex_of_string block_hash) 
      >> return { acc with count = (acc.count + 1);  utxos = utxos;  }
    )
*)

(*    return { acc with count = (acc.count + 1); (* utxos = utxos; *) } *)
  in

	Db.open_db "mydb" >>= fun db -> 
  Lwt_unix.openfile "blocks.dat"  [O_RDONLY] 0 >>= fun fd -> 

    match Lwt_unix.state fd with 
      Opened -> 
        write_stdout "scanning blocks..." 
        >> loop_blocks fd f db 0 
(*        >>= fun acc ->   
          write_stdout ("result " ^ (string_of_int acc.count  ))
 *)       >> Lwt_unix.close fd
      | _ -> 
        write_stdout "couldn't open file"
  )

let () = run ()

