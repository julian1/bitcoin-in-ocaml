
(* Lwt wrapper around level db 
	- note that iterator stuff is not thread safe, but get, put etc are
*)

let (>>=) = Lwt.(>>=)
let return  = Lwt.return

let detach f = 
  Lwt_preemptive.detach 
    (fun () -> f ) () 

type t = LevelDB.db

let open_db n = detach @@ LevelDB.open_db n 

let get_key i = detach @@ LevelDB.Iterator.get_key i
let get_value i = detach @@ LevelDB.Iterator.get_value i

(* shouldn't really use this *)
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
 

  (** [seek it s off len] seeks to first binding whose key is >= to the key
    * corresponding to the substring of [s] starting at [off] and of length
    * [len].
*) 

let seek i key = detach @@ LevelDB.Iterator.seek i key 0 (String.length key) 

