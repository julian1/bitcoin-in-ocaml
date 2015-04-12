

(* ok this thing needs access to some of the shared data structures
namely my_event *)

open Misc


(* let manage_chain (state : Misc.my_app_state ) (e : Misc.my_event)  =   *)
 let manage_chain state  e   =  

  match e with
    
    | GotMessage (conn, header, raw_header, payload) -> 
	(
      match header.command with
        | "inv" ->
          { state with 
            jobs = state.jobs @ [
              log @@ format_addr conn ^ " chainstate got inv"
            ]
          }
		| _ -> state
		
		)
    | _ -> state

(* we've got an issue about adding read jobs - can only do it once *)

