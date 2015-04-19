(*
	like a fold, with an initial and transition/update function
*)

type t

val create : unit -> t Lwt.t

(*
	needs the event message...

*)

val update : Misc.my_event -> t Lwt.t


(*
val print : t -> unit 
*)

