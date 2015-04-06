
(*
  - index may be reverse of what we want
  - do we record the value?  or refer ba
  - we should wrap the actions to be simpler... 
  - we can easily make a fold type function with the iterator.
  - mostly we want to store the lseek pos into the blocks 

  - note leveldb iterators are not thread safe.
*)

module M = Message

let (>>=) = Lwt.(>>=)
let return = Lwt.return



let write_stdout = Lwt_io.write_line Lwt_io.stdout 

let run () = 
	Lwt_main.run ( 
    let rec loop i = 
      Db.get_keyval i >>= fun (key,value) -> 
        let k = Index.decodeKey key in 
        let v = Index.decodeValue value in 
        (* let pos, hash = M.decodeHash32 key 0 in 
        let pos, index = M.decodeInteger32 key pos in *)
        write_stdout @@ M.hex_of_string k.hash ^ " " ^ string_of_int k.index ^ " " ^ v.status ^ " " ^ string_of_int v.lseek 
      >> Db.next i 
      >> Db.valid i >>= fun valid -> 
        if valid then  
          loop i 
        else
          return ()
    in
    Db.open_db "mydb" >>= fun db -> 
    Db.make db >>= fun i -> Db.seek_to_first i 
    >> 
    loop i
  )

let () = run ()

