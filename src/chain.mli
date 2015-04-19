(*
	like a fold, with an initial and transition/update function
*)

type t

val create : unit -> t Lwt.t

(*
	needs the event message...

	ok, we want to return a tuple...
*)

val update : Misc.my_event -> (t * Misc.jobs_type )  


(*
val print : t -> unit 
*)

