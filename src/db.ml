


let (>>=) = Lwt.(>>=)
let return  = Lwt.return

let detach f = 
  Lwt_preemptive.detach 
    (fun () -> f ) () 



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

let put db key value = detach @@ LevelDB.put db key value 


let get db key = detach @@ LevelDB.get db key 
 

 


