
module U = Misc
module S = String
module M = Message

let (>>=) = Lwt.(>>=)
let return = Lwt.return

let log s = U.write_stdout s >> return U.Nop


(* transfer a sequence job into the jobs list 
    rather than seq_job_running to false when finished we should wrap
    it in a closure here...



    OK, i think there's an issue that when one job is complete
    we're not immediately scheduling the next one... 
  
*)

let (>>=) = Lwt.(>>=) 
let return = Lwt.return


let log s = U.write_stdout s >> return U.Nop

let update3 e (state : Misc.my_app_state) =
  match e with 
    | Misc.SeqJobFinished -> { state with seq_job_running = false;  
      (* jobs = state.jobs @  [ log @@ "seq job finished"   ] *) }
    | _ -> state
  
 

let update2 _ (state : Misc.my_app_state) =
  if not state.seq_job_running 
    && state.seq_jobs_pending <> Myqueue.empty 
  then
    let job,t = Myqueue.take state.seq_jobs_pending in
    { state with
      seq_job_running = true;
      seq_jobs_pending = t;
      jobs = (
        job () 
        (* >> U.write_stdout "here"  *)
        >> return Misc.SeqJobFinished)
        :: state.jobs;
    }
  else
    state


let update state e =
  state |> update3 e |> update2 e




(* - we allow networking to run independenly of the sequenced io actions. 
  this allows blocks to always be accepted when they are received, even if cannot process immediately
  - block is still provisional, if it fails a check we can coordinate to remove from p2p heads
*) 

(*

let update1 e (state : Misc.my_app_state) =
	match e with
	| U.GotBlock (hash, height, raw_header, payload) ->
    let y () =
      (* save block *)
   		let s = raw_header ^ payload in
      let len = S.length s in
		  Lwt_unix.lseek state.blocks_fd 0 Unix.SEEK_END
		  >>= fun pos -> Lwt_unix.write state.blocks_fd s 0 len 
		  (* check here, if return short then throw ? *)
		  >>= fun count ->
        if count <> len then
          raise (Failure "uggh")
        else
      (* save index *)
(*
			Db.put state.db ("block/" ^ hash ^ "/pos") (M.encodeInteger64 (Int64.of_int pos))
			>> log @@ String.concat "" [
            "saved block - ";
            "hash: "; (M.hex_of_string hash);
            " height: "; string_of_int height ;
				    " pos: " ^ string_of_int pos;
        ]
      >> *) return U.SeqJobFinished
    in
    { state with
      seq_jobs_pending = Myqueue.add state.seq_jobs_pending y
    }

  (* this shouldn't really be here, it's general sequencing *)
  | U.SeqJobFinished ->
    { state with
      seq_job_running = false;
   }
	| _ -> state
*)

(*
  - Important a block may arrive out of order - but we'll reject it.

  - now we write the block to disk.
  - then we'll update lseek position in heads structure ... no we'll store on disk
  - then we'll determine the chain with most pow (should be easy)
  - and we'll have the last position already recorded
  - so we'll compute common fork point
  - and set of index update actions

  finding the common fork point ought to be easy. just a set, and start, adding items.

  laziness would be nice in tracing back the paths to the common fork point.
*)

