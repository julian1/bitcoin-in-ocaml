
(*
  - index may be reverse of what we want
  - do we record the value?  or refer ba
 
  - we should wrap the actions to be simpler... 

  - we can easily make a fold type function with the iterator.
  - mostly we want to store the lseek pos into the blocks 
*)
module M = Message

let (>>=) = Lwt.(>>=)
let return  = Lwt.return

let detach f = 
  Lwt_preemptive.detach 
    (fun () -> f ) () 


module DB = 
  struct
	  let open_db n = detach @@ LevelDB.open_db n 

    (* let get_key i = detach @@ LevelDB.Iterator.get_key i
    let get_value i = detach @@ LevelDB.Iterator.get_value i
    *)

    let get_keyval i = detach @@ (
      let key = LevelDB.Iterator.get_key i  in
      let value = LevelDB.Iterator.get_value i in 
      (key, value)
    )

	  let make db = detach @@ LevelDB.Iterator.make db 
	  let seek_to_first i = detach @@ LevelDB.Iterator.seek_to_first i 
		let next i = detach @@ LevelDB.Iterator.next i 
    let valid i = detach @@ LevelDB.Iterator.valid i
  end 



(* combine to return key,val *)



let write_stdout = Lwt_io.write_line Lwt_io.stdout 

let run () = 

	Lwt_main.run (

	let rec loop i = 
    DB.get_keyval i >>= fun (key,value) -> 
      let pos, hash = M.decodeHash32 key 0 in 
      let pos, index = M.decodeInteger32 key pos in 
			write_stdout @@ M.hex_of_string hash ^ " " ^ string_of_int index ^ " " ^ value
		>> DB.next i 
    >> DB.valid i >>= fun valid -> 
      if valid then  
        loop i 
      else
        return ()
	in
	DB.open_db "mydb" >>= fun db -> 
	DB.make db >>= fun i -> DB.seek_to_first i 
	>> 
	loop i
)


let () = run ()

