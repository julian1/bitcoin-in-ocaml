
(* scan blocks and compute tx indexes *)

(*
  ------
  Think that we need a module interface, then we can use leveldb or sqllite behind it... 
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
  let process_output tx_hash index (output : M.tx_out) = 
    let key = I.encodeKey { hash = tx_hash; index = index } in 
    let value = I.encodeValue { 
        status = "u"; block_pos = block_pos; tx_pos = tx.pos; tx_length = tx.length;  
        output_pos = output.pos; output_length = output.length; 
      } in
    Db.put db key value
  in
  (* process in parallel inputs, then outputs in sequence *) 
  Lwt.join ( L.map process_input tx.inputs )
  >> Lwt.join ( L.mapi (process_output hash) tx.outputs )

*)
  
(* this thing doesn't use the db, so it should be configured ...  all this stuff is still mucky *)

let process_tx block_pos ((hash, tx) : string * M.tx )  = 
	return ()
 

let process_block payload pos count = 
    let block_pos = pos in
    let block_hash = M.strsub payload 0 80 |> M.sha256d |> M.strrev in
    (* decode tx's and get tx hash *)
    let pos = 80 in
    let pos, tx_count = M.decodeVarInt payload pos in 
    let _, txs = M.decodeNItems payload pos M.decodeTx tx_count in
    let txs = L.map (fun (tx : M.tx) -> 
      let hash = M.strsub payload tx.pos tx.length |> M.sha256d |> M.strrev 
      in hash, tx
    ) txs 
    in
	(* we should not be doing this here - in this general purpose function *)
    if count mod 1000 = 0 then 
      Misc.write_stdout @@ string_of_int count ^ " " ^ M.hex_of_string block_hash
      (* >> write_stdout @@ string_of_int pos  *)
    else 
      return ()
    ;
    >>
    L.fold_left (fun a b ->  
       a >> process_tx block_pos b ) 
      (return ())  
      txs  



(* we don't really need the count *) 


let loop_blocks fd f =
	let rec loop_blocks' count =
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
			  loop_blocks' (succ count)
		  )
  in loop_blocks' 0 

in
	 

let () = 
  Lwt_main.run (



    Lwt_unix.openfile "blocks.dat" [O_RDONLY] 0 >>= fun fd -> 
      match Lwt_unix.state fd with 
        Opened -> 
          Misc.write_stdout "scanning blocks..." 
          >> loop_blocks fd process_block 
          >> Lwt_unix.close fd
        | _ -> 
          Misc.write_stdout "couldn't open file"
)
in ()




