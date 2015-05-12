
module U = Misc
module S = String


let (>>=) = Lwt.(>>=) 
let return = Lwt.return

(* 
  - Important a block may arrive out of order - but we'll reject it.
  - we've already recorded in heads, which means if the save fails 
it will all go wrong 
  
  

  - now we write the block to disk.
  - then we'll update lseek position in heads structure ... 

  - then we'll determine the chain with most pow (should be easy)
  - and we'll have the last position already recorded
  - so we'll compute common fork point 
  - and set of index update actions

  - it doesn't actually matter when we do the block save, it just has to be before
  the actual chain update, when we have to potentially read it again... 
*) 

let log s = U.write_stdout s >> return U.Nop

let update (state : Misc.my_app_state) e = 
	match e with 
	| U.GotBlock (raw_header, payload) -> 
    { state with jobs = state.jobs @ [
		
      (* write the block to disk *) 	
		  log "whoot got block"; 

			let s = raw_header ^ payload in 
		  Lwt_mutex.lock state.blocks_fd_m
		  >> Lwt_unix.lseek state.blocks_fd 0 Unix.SEEK_END 
		  >>= fun pos -> Lwt_unix.write state.blocks_fd s 0 (S.length s) 
		  (* check here, if return short then throw ? *)
		  >>= fun count -> let () = Lwt_mutex.unlock state.blocks_fd_m in 
        if count <> S.length s then 
          raise (Failure "uggh")
        else
          return @@ U.SavedBlock ("test", pos )
	] }


  | U.SavedBlock (hash, pos) -> 
    { state with jobs = state.jobs @ [
  	  log @@ "whoot saved block " ^ string_of_int pos; 
  ]}

	| _ -> state


