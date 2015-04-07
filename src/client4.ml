
(* we should record the value, if only to verify ordering *)

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

let (>|=) = Lwt.(>|=)


module M = Message
module I = Index

module L = List
module CL = Core.Core_list

(*
let sequence txs  = 
  L.fold_left (fun a b ->  
         b >> a ) 
        (return ())  
        (List.rev txs ) 
*)

(* if we want the position, then we need the lseek position 

  O
*) 

(* : int * int * string * M.tx  *) 
let decodeTXXX payload =
    (* move to Message ? *)
    let pos = 80 in
    let pos, tx_count = M.decodeVarInt payload pos in 
    (* decode txs *)
(*    let first = pos in *)
    let _, txs = M.decodeNItems payload pos M.decodeTx tx_count in
    (* extract tx start/lengths 
        we kind of want the output scripts to be indexed?  
    *)
(*    let lens = L.map (fun (tx : M.tx) -> tx.bytes_length) txs in 
    let poss = L.fold_left (fun acc len -> L.hd acc + len::acc) [first] lens |> L.tl |> L.rev in 
    let zipped = CL.zip_exn poss lens in
    let zipped = CL.zip_exn zipped txs in
    L.map (fun ((pos,len),tx) -> 
      let hash = M.strsub payload pos len |> M.sha256d |> M.strrev 
      in pos, len, hash, tx
      ) 
    zipped 
*)
    L.map (fun (tx : M.tx) -> 
      let hash = M.strsub payload tx.pos tx.len |> M.sha256d |> M.strrev 
      in hash, tx
    ) txs 



(*
  al right we need to pass in the block position as well...
  to record the lseek pos.
*)

(* so we can push this stuff out into db, or somewhere else so it's useable by client 

  in fact we could hide the entire thing behind a module and just have
  get 
and set
*) 

       (* a >> process_tx db pos b ) *) 

(* let process_tx db ((pos, len, hash, tx) : int * int * string * M.tx ) *)
(* let process_tx db block_pos ((pos, length , hash, tx) : int * int * string * M.tx )  =  *)
let process_tx db block_pos ((hash, tx) : string * M.tx )  = 
  let coinbase = M.zeros 32 
  in
  let process_input (input : M.tx_in) = 
    if input.previous = coinbase then
      return ()
    else
      let key = I.encodeKey { hash = input.previous; index = input.index  } in
      Db.get db key 
      >>= (fun result ->	
        match result with 
          Some s -> 
            (* we should write functions to do this *)
            let value = I.decodeValue s in 
            if value.status <> "u" then  
              let msg = "ooops tx is spent" in 
              raise (Failure msg) 
            else
              Db.put db key (I.encodeValue { value with status = "s";  } ) 
          | None -> 
            let msg = "txo not found " ^ M.hex_of_string input.previous ^ " " ^ string_of_int input.index in
            raise (Failure msg) 
      ) 
  in
  let process_output hash index _  = 
    let key = I.encodeKey { hash = hash; index = index } in 
    let value = I.encodeValue { status = "u"; lseek = block_pos + tx.pos; length = tx.len;  } in
    Db.put db key value
  in
  (* process in parallel inputs, then outputs in sequence *) 
  Lwt.join ( L.map process_input tx.inputs )
  >> Lwt.join ( L.mapi (process_output hash) tx.outputs )


  
  (* this thing doesn't use the db, so it should be configured ...
    all this stuff is still mucky
  *)
 let process_block db payload pos count = 
    let block_hash = M.strsub payload 0 80 |> M.sha256d |> M.strrev in
    let txs = decodeTXXX payload in
    if count mod 1000 = 0 then 
      Misc.write_stdout @@ string_of_int count ^ " " ^ M.hex_of_string block_hash
      (* >> write_stdout @@ string_of_int pos  *)
    else 
      return ()
    ;
    >>
    L.fold_left (fun a b ->  
       a >> process_tx db pos b ) 
      (return ())  
      txs  



let run () = 

  Lwt_main.run (

    (* we don't really need the count 
        but we do need the file descriptor...
    *) 
    let rec loop_blocks fd f count =
      Misc.read_bytes fd 24
      >>= fun x -> match x with 
        | None -> return ()
        | Some s -> ( 

          Lwt_unix.lseek fd 0 SEEK_CUR >>= fun pos ->  

          let _, header = M.decodeHeader s 0 in
          (* Lwt_io.write_line Lwt_io.stdout @@ header.command ^ " " ^ string_of_int header.length >> *) 

          Misc.read_bytes fd header.length 
          >>= fun u -> match u with 
            | None -> return ()
            | Some payload -> 
              f payload pos count 
              >> 
              loop_blocks fd f (count + 1)
          )
    in 

    Db.open_db "mydb" >>= fun db -> 
    Lwt_unix.openfile "blocks.dat" [O_RDONLY] 0 >>= fun fd -> 
      match Lwt_unix.state fd with 
        Opened -> 
          Misc.write_stdout "scanning blocks..." 
          >> loop_blocks fd (process_block db) 0 
          >> Lwt_unix.close fd
        | _ -> 
          Misc.write_stdout "couldn't open file"
)

let () = run ()



