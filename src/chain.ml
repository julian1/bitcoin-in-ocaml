
let (>>=) = Lwt.(>>=)
let return = Lwt.return



(* val create : unit -> t Lwt.t  *)

type t = int 

let write_stdout = Lwt_io.write_line Lwt_io.stdout
 
let create () = 
	(* is an io function *)

	write_stdout "**** CREATE " 
	>>
	(return 123 )


let log s = Misc.write_stdout s >> return Misc.Nop


let update (a : t) (e : Misc.my_event ) = 
	let jobs = [ log "whoot"  
	] in
	(123, jobs) 




