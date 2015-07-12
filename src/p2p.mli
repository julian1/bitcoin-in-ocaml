
(*
	- information about peers might be something to hide in a type
	- max number of peers etc.

  - we need the ability to cancel, close connections that are doing a read...
    if nothing's read or they're too slow.
    metrics...
*)
val create : unit -> Misc.jobs_type


(* should be able to constrain the last type more *)
val update : Misc.my_app_state -> Misc.my_event -> (Misc.my_event ) Lwt.t


