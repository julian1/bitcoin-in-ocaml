
let (>>=) = Lwt.(>>=)
let return = Lwt.return

module M = Message


let y () = (return Misc.SeqJobFinished) 



let run f =

  Lwt_main.run (

	
	Db.open_db "mydb" 
	>>= fun db ->  

    Chain.create () 
    >>= fun result ->   

    match result with 
      Some (tree, blocks_m, blocks_fd) -> (
        (* we actually need to read it as well... as write it... *)
        let state =
          ({
			jobs = P2p.create();
            connections = [];
			heads = tree;
(*			blocks_fd_m = blocks_m; *)
			blocks_fd = blocks_fd;

			(* should be hidden ?? *)
			block_inv_pending  = None; 
			blocks_on_request = Misc.SS.empty; 
			last_block_received_time = [];

			seq_jobs_pending =  [ y ] ;
			seq_job_running = false;

			db = db;

          } : Misc.my_app_state )
        in

        let rec loop (state : Misc.my_app_state ) =
          Lwt.catch (
          fun () -> Lwt.nchoose_split state.jobs

            >>= fun (complete, incomplete) ->
              (*Lwt_io.write_line Lwt_io.stdout  @@
                "complete " ^ (string_of_int @@ List.length complete )
                ^ ", incomplete " ^ (string_of_int @@ List.length incomplete)
                ^ ", connections " ^ (string_of_int @@ List.length state.connections )
            >>
          *)
              let state = List.fold_left f { state with jobs = incomplete } complete in 
              if List.length state.jobs > 0 then
                loop state
              else
                Lwt_io.write_line Lwt_io.stdout "finishing - no more jobs to run!!"
                >> return ()
          )
            (fun exn ->
              (* must close *)
              let s = Printexc.to_string exn  ^ "\n" ^ (Printexc.get_backtrace () ) in
              Lwt_io.write_line Lwt_io.stdout ("finishing - exception " ^ s )
              >> (* just exist cleanly *)
                return ()
            )
        in
          loop state
        )
  )


let f state e =
  let state = P2p.update state e in
  let state = Chain.update state e in 
  let state = Blocks.update state e in 
  state

(*
  { state with 
    chain = chain; 
    connections = connections; 
    jobs =  state.jobs @ jobs1 @ jobs2  
  }
*)

let () = run f



