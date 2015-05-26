(*
	like a fold, with an initial and transition/update function
*)

(* type t *)

(* val create : unit -> option Lwt.t *)
val create : unit -> 
  (Misc.my_head Misc.SS.t 
  * Lwt_unix.file_descr 
	)
	option Lwt.t
 

(*
val update : t -> Misc.connection list -> Misc.my_event -> (t * Misc.jobs_type)  
*)
val update : Misc.my_app_state -> Misc.my_event -> Misc.my_app_state 


(*
val get_heads : t -> Misc.my_head Misc.SS.t 
val get_jobs : t -> Misc.jobs_type  
*)

(*
	- we have to also get the descriptor for the block to be able to read it...
	- and the mutex	

	basically everything except, 

	not-shared 
		-  block_inv_pending, blocks_on_request, last_block_received_time 
	shared 
		-  heads , blocks_fd_m,  blocks_fd 
 
	- creating silly accessors. to share structure is dumb. 
	- what about hiding stuff that can be hidden in
	- and sharing... what needs to be shared in
	top level?	


	so we have a single top level structure.
	think this is good,	

	so we'll pass ... 
*)

