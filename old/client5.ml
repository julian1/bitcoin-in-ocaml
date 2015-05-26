(*
  scan tx indexes, and lookup the relevant tx or output and print 

  - we can easily make a fold type function with the iterator.
  - note leveldb iterators are not thread safe.
*)

(*
  so if we want to use the same leveldb to perform indexing, then we need to be
  able to move to the first item...
*)

(*
  indexing addresses is harder because they can be used multiple times . 
  we have to use either 
      - a value representing which particular tx and index 
      which means we can't test whether an address exists without
      using an iterator. 

      - or we just use sequential number which means need to scan 
      to the end to update.

      - or else use a big list, which has problem that must retrieve all 
      uses, add an entry and store back again. 

      - in fact the first case is effieicent... for indexing
        but search is slow with the index ...
      - address / tx / output index

      ---- 
      note that we could just the same value for the mention. 

      In fact the indexing action, is almost exactly the same... as for txo
      except we append a bit the address to the start of the key.

*)

module M = Message

let (>>=) = Lwt.(>>=)
let return = Lwt.return


(* is the iterator valid and does the first char match 
  we could actually return the key
  *)
let valid i = 
  Db.valid i 
  >>= fun valid -> 
    if valid then  
      Db.get_key i 
      >>= fun key -> 
        return (key.[0]) (* = 't')  *)
    else
      return '0'


let run () = 
	Lwt_main.run ( 

    let rec loop i fd = 
      Db.get_keyval i >>= fun (key,value) -> 
        let k = Index.decodeKey key in 
        let v = Index.decodeValue value in 
        Misc.write_stdout @@ M.hex_of_string k.hash ^ " " ^ string_of_int k.index 
          ^ " " ^ Index.formatValue v
        (*
          Lwt_unix.lseek fd (v.block_pos + v.tx_pos ) SEEK_SET
          >> Misc.read_bytes fd v.tx_length 
          >>= (fun x -> match x with 
            | None -> return ()
            | Some payload ->  
              (*Misc.write_stdout (Misc.string_of_bytes payload ) *)
              let _, tx = M.decodeTx payload 0 in 
              Misc.write_stdout  (M.formatTx tx) 
          )
      *)
        >> Lwt_unix.lseek fd (v.block_pos + v.output_pos ) SEEK_SET
        >> Misc.read_bytes fd v.output_length 
        >>= (fun x -> match x with 
          | None -> return ()
          | Some payload ->  
            let _, output = M.decodeTxOutput payload 0 in
            let tokens = M.decode_script output.script in 
            Misc.write_stdout @@ M.formatTxOutput output
            >> Misc.write_stdout @@ M.format_script tokens 
            >> Misc.write_stdout "" 
        )

      (* need to partition the keyspace, - instead of doing if else we could pass the function?? *)
      >> Db.next i 
      >> valid i >>= fun ret ->
        if ret = 't' then
          loop i fd 
        else
          return ()
    in
    Lwt_unix.openfile "blocks.dat" [O_RDONLY] 0 
    >>= fun fd -> 
      match Lwt_unix.state fd with 
        Opened -> 
          Misc.write_stdout "opened blocks..." 
          >> Db.open_db "mydb" 
          >>= fun db -> Db.make db 
          (* >>= fun i -> Db.seek_to_first i  *)
          >>= fun i -> Db.seek i "t"  
          >> loop i fd
        | _ -> 
          Misc.write_stdout "failed to open file..." 
  )

let () = run ()

