(*
	like a fold, with an initial and transition/update function
	- maybe change name, since job is to download
*)

val create : unit -> 
   Lwt_unix.file_descr Lwt.t
 

val update : Misc.my_app_state -> Misc.my_event -> Misc.my_app_state 

