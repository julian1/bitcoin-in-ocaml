
(*
  - index may be reverse of what we want
  - do we record the value?  or refer ba
 
  - we should wrap the actions to be simpler... 

  - we can easily make a fold type function with the iterator.
  - mostly we want to store the lseek pos
*)
module M = Message

let (>>=) = Lwt.(>>=)
let return  = Lwt.return

let detach f = 
  Lwt_preemptive.detach 
    (fun () -> f ) () 


module SSS = 
  struct
    let get_key i = detach @@ LevelDB.Iterator.get_key i
    let get_value i = detach @@ LevelDB.Iterator.get_value i
  end 



(* combine to return key,val *)



let write_stdout = Lwt_io.write_line Lwt_io.stdout 

let run () = 

	Lwt_main.run (

	let rec loop i = 
		(* need to decode the index *)
		SSS.get_key i 
		>>= fun key -> 
		SSS.get_value i 
		>>= fun value -> 
      let pos = 0 in
      let pos, hash = M.decodeHash32 key pos in 
      let pos, index = M.decodeInteger32 key pos in 

			write_stdout @@ M.hex_of_string hash ^ " " ^ string_of_int index ^ " " ^ value
		>> detach @@ LevelDB.Iterator.next i 

    >> if LevelDB.Iterator.valid i then  
      loop i 
    else
      return ()



	in
	detach @@ LevelDB.open_db "mydb" 
	>>= fun db -> 
	detach @@ LevelDB.Iterator.make db 
	>>= fun i -> 
	detach @@ LevelDB.Iterator.seek_to_first i 
	>> 
	loop i
)


let () = run ()

