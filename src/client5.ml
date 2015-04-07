
(*
  - index may be reverse of what we want
  - do we record the value?  or refer ba
  - we should wrap the actions to be simpler... 
  - we can easily make a fold type function with the iterator.
  - mostly we want to store the lseek pos into the blocks 

  - note leveldb iterators are not thread safe.

  - all right now we should see if we can actually get lookup the tx,

  - none of this tx stuff is needed...
*)

module M = Message

let (>>=) = Lwt.(>>=)
let return = Lwt.return



let write_stdout = Lwt_io.write_line Lwt_io.stdout 

let run () = 
	Lwt_main.run ( 

    let rec loop i fd = 
      Db.get_keyval i >>= fun (key,value) -> 
        let k = Index.decodeKey key in 
        let v = Index.decodeValue value in 
        write_stdout @@ M.hex_of_string k.hash ^ " " ^ string_of_int k.index 
          ^ " " ^ v.status ^ " " ^ string_of_int v.lseek 
          ^ " " ^ string_of_int v.length 
      >> Db.next i 
      >> Db.valid i >>= fun valid -> 
        if valid then  

          Lwt_unix.lseek fd v.lseek SEEK_SET
          >> Misc.read_bytes fd v.length 
          >>= (fun x -> match x with 
            | None -> return ()
            | Some payload ->  
              (*write_stdout (Misc.string_of_bytes payload ) *)
              let _, tx = M.decodeTx payload 0 in 
              (* write_stdout tx.value  (M.formatTx tx)  *) 
              loop i fd 
          )
        else
          return ()
    in
    Lwt_unix.openfile "blocks.dat" [O_RDONLY] 0 >>= fun fd -> 
      match Lwt_unix.state fd with 
        Opened -> 
          write_stdout "opened blocks..." 
          >> Db.open_db "mydb" 
          >>= fun db -> Db.make db 
          >>= fun i -> Db.seek_to_first i 
          >> loop i fd
  )

let () = run ()

