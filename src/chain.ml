
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


let update a e  = 
  match e with
    | Misc.Nop -> (a, [] )

	| _ -> 
	let jobs = [ 
		write_stdout "hi" >> return Misc.Nop
		(*log "whoot2"  *)
	] in
	(123, jobs) 




