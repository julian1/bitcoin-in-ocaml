(*
  scan tx indexes, and lookup the relevant tx or output and print 

  - we can easily make a fold type function with the iterator.
  - note leveldb iterators are not thread safe.
*)

(*
  so if we want to use the same leveldb to perform indexing, then we need to be
  able to move to the first item...
*)

module M = Message

let (>>=) = Lwt.(>>=)
let return = Lwt.return



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
 
      >> Db.next i 
      >> Db.valid i 
      >>= fun valid -> 
        if valid then  
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
          >>= fun i -> Db.seek_to_first i 

          >> Db.seek i (M.encodeHash32 ( M.string_of_hex "222c3b911469a8ea1f0eff0dd34b4facdd5db7e0ebe47d97ac08cbed2bc00200" )) 
          >> loop i fd
        | _ -> 
          Misc.write_stdout "failed to open file..." 
  )

let () = run ()

