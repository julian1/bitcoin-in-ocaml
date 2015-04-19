(*
	like a fold, with an initial and transition/update function
*)

type t

val create : unit -> t Lwt.t

val update : t -> Misc.my_event -> (t * Misc.jobs_type )  


