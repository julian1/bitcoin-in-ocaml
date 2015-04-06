
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

let (>>=) = Lwt.(>>=)
let return = Lwt.return



module M = Message
module L = List
module CL = Core.Core_list

(* if we want the position, then we need the lseek position 

*) 

(* : int * int * string * M.tx  *) 
let decodeTXXX payload =
    (* move to Message ? *)
    let pos = 80 in
    let pos, tx_count = M.decodeVarInt payload pos in 
    (* decode txs *)
    let first = pos in
    let _, txs = M.decodeNItems payload pos M.decodeTx tx_count in
    (* extract tx start/lengths *)
    let lens = L.map (fun (tx : M.tx) -> tx.bytes_length) txs in 
    let poss = L.fold_left (fun acc len -> L.hd acc + len::acc) [first] lens |> L.tl |> L.rev in 
    let zipped = CL.zip_exn poss lens in
    let zipped = CL.zip_exn zipped txs in
    L.map (fun ((pos,len),tx) -> 
      let hash = M.strsub payload pos len |> M.sha256d |> M.strrev 
      in pos, len, hash, tx
      ) 
    zipped 



let write_stdout = Lwt_io.write_line Lwt_io.stdout 

(*
  al right we need to pass in the block position as well...
  to record the lseek pos.
*)

(* let process_tx db ((pos, len, hash, tx) : int * int * string * M.tx ) *)
let process_tx db ((pos, len, hash, tx) : int * int * string * M.tx )  = 
  let coinbase = M.zeros 32 
  in
  let f (input : M.tx_in) = 
    if input.previous = coinbase then
      return ()
    else
      let key = (M.encodeHash32 input.previous ^ M.encodeInteger32 input.index) in
      Db.get db key 
      >>= (fun result ->	
          match result with 
            Some _ -> return () (* write_stdout ("found " ^ s)  *)
            | None -> 
              let msg = "txo no found " ^ M.hex_of_string input.previous ^ " " ^ string_of_int input.index in
              raise (Failure msg) 
        ) 
      >>
      Db.put db key "s" 
  in
  (* fold over inputs *) 
	Lwt.join @@ 
  ( L.map f tx.inputs )
  >>
    Lwt.join @@ 
      L.mapi (fun i _ -> 
        Db.put db (M.encodeHash32 hash ^ M.encodeInteger32 i) "u" ) tx.outputs 



 let process_block payload db count = 
    let block_hash = M.strsub payload 0 80 |> M.sha256d |> M.strrev in
    let txs = decodeTXXX payload in
      Lwt.join (
        L.map 
          ( process_tx db) 
          txs  
      )
    >> 
      write_stdout @@ string_of_int count ^ " " ^ M.hex_of_string block_hash
(*      >> if count mod 1000 = 0 then 
      write_stdout @@ string_of_int count ^ " " ^ M.hex_of_string block_hash
    else 
      >> return ()
*)




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
              >> 
              loop_blocks fd f db (count + 1)
          )
    in 

    Db.open_db "mydb" >>= fun db -> 
    Lwt_unix.openfile "blocks.dat" [O_RDONLY] 0 >>= fun fd -> 
      match Lwt_unix.state fd with 
        Opened -> 
          write_stdout "scanning blocks..." 
          >> loop_blocks fd process_block db 0 
          >> Lwt_unix.close fd
        | _ -> 
          write_stdout "couldn't open file"
)

let () = run ()



