
module U = Misc
module S = String
module M = Message


let (>>=) = Lwt.(>>=) 
let return = Lwt.return

(* 
  - Important a block may arrive out of order - but we'll reject it.
  - we've already recorded in heads, which means if the save fails 
it will all go wrong 
  
  - block is still provisional, we can remove it from heads... if it fails a test...  

  - now we write the block to disk.
  - then we'll update lseek position in heads structure ... 

  - then we'll determine the chain with most pow (should be easy)
  - and we'll have the last position already recorded
  - so we'll compute common fork point 
  - and set of index update actions

  - it doesn't actually matter when we do the block save, it just has to be before
  the actual chain update, when we have to potentially read it again... 

  - string best_pow_hash.  
  finding the common fork point ought to be easy. just a set, and start, adding items. 

  hmmm but to do it quickly...
  laziness would be nice in tracing back the paths to the common fork point. 

  - is there any guarantee that writes will be ordered, as they extend the chain?  
      they will need to be, to detect valid fork points (possibly because obtaining the 
      obviously the pos advances) 

  - i think we almost certainly want to record the height.

  - we'll have a fold between current, new and just walk back prev, alternating even/odd. 
*) 

let log s = U.write_stdout s >> return U.Nop

let update (state : Misc.my_app_state) e = 
	match e with 
	| U.GotBlock (hash, height, raw_header, payload) -> 
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

			Db.put state.db "key" "value" 
			>>
			  log @@ "saved block - " ^ " hash " ^ (M.hex_of_string hash) ^ " height " ^ string_of_int height ; 
	] }


	| _ -> state


