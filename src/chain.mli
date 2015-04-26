(*
	like a fold, with an initial and transition/update function
*)

type t

val create : unit -> t option Lwt.t

val update : t -> Misc.connection list -> Misc.my_event -> (t * Misc.jobs_type)  


