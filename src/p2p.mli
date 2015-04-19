
(*
	information about peers might be something to hide in a type
*)
val create : unit -> Misc.jobs_type 

val update : Misc.connection list -> Misc.my_event -> (Misc.connection list * Misc.jobs_type)  


