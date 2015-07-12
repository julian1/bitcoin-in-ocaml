(*
	like a fold, with an initial and transition/update function
	- maybe change name, since job is to download
*)

(*
val create : unit -> 
   Lwt_unix.file_descr Lwt.t
*) 

(*
  TODO reintroduce the abstract t 
*)
(*
val update : Misc.my_app_state -> Misc.my_event -> Misc.my_app_state 
*)

(* should be able to constrain the last type more *)
val update : Util.my_app_state -> Util.my_event -> (Util.my_event ) Lwt.t



