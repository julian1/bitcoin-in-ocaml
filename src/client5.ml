
open Lwt (* for >>= *)
module M = Message


let detach f = 
  Lwt_preemptive.detach 
    (fun () -> f ) () 


let write_stdout = Lwt_io.write_line Lwt_io.stdout 

let run () = 

	Lwt_main.run (

	let rec loop i = 
		(* need to decode the index *)
		detach @@ LevelDB.Iterator.get_key i 
		>>= fun key -> 
		detach @@ LevelDB.Iterator.get_value i 
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

