
let (>>=) = Lwt.(>>=)
let return = Lwt.return



(* val create : unit -> t Lwt.t  *)

type t = int 

let write_stdout = Lwt_io.write_line Lwt_io.stdout
 
let create () = 

	write_stdout "**** CREATE " 
	>>
	(return 123 )


let update (e : Misc.my_event ) = 

	write_stdout "**** update " 
	>>
	(return 123 )




