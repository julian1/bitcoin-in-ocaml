(*
  corebuild -I src  -package pgocaml,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax src/client.byte
*)

let (>>=) = Lwt.(>>=)
let return = Lwt.return

module M = Message
module U = Misc


let log s = U.write_stdout s

let run () =

  Lwt_main.run (

    (* we'll have to think about db transactions *) 
    log "connecting and create db"
    >> U.PG.connect ~host:"127.0.0.1" ~database: "test" ~user:"meteo" ~password:"meteo" ()
    >>= fun db ->

      Processblock.create_prepared_stmts db 
    >>

      Chain.create ()
    >>= fun blocks_fd -> 
       (
        (* we actually need to read it as well... as write it... *)
        let state =
          ({
			jobs = P2p.create();
            connections = [];
			(*heads = tree; *)

            db = db; 
			blocks_fd = blocks_fd;

			(* should be hidden ?? *)
			block_inv_pending  = None;
			blocks_on_request = U.SS.empty;
			last_block_received_time = [];

			seq_jobs_pending = Myqueue.empty;
			seq_job_running = false;

          } : U.my_app_state )
        in



        let rec loop (state : U.my_app_state ) queue  =
          Lwt.catch (
          fun () -> Lwt.nchoose_split state.jobs

            >>= fun (complete, incomplete) ->
              (*Lwt_io.write_line Lwt_io.stdout  @@
                "complete " ^ (string_of_int @@ List.length complete )
                ^ ", incomplete " ^ (string_of_int @@ List.length incomplete)
                ^ ", connections " ^ (string_of_int @@ List.length state.connections )
              >>
              *)
              (*
                - so rather than fold_left we should just push the event onto the queue... 
                - we want a queue, and we'll just push events onto the queue... 
                - and the sequencer will pull jobs from the queue 
                - hang on. a kkk
                  GotMessage is not a job...
                  we should process jobs. (that might be actions)

                - actually i think it would be nice if it was almost the same interface

                - the sequencer should take the state and the event and run an io action.
                - issue about how to start 

                - a seq job that cmpletes should return NOP .  since it has state it can 
                  set that the job is complete. 
              *)
              (*
                if we only process one element in the queue at a time, 
                - then to keep the queue ticking over we should bind completion the io jobs 
                even if they don't run an io action...
                - or 
              *)

              let state = { state with jobs = incomplete } in

              (* process io events *) 
              let f (state,queue) e = 
                match e with 
                  | U.Nop -> state,queue
                    (* a seq job finished then take the new state *) 
                  | SeqJobFinished -> state,queue
                    (* any other event gets added to queue *)
                  | _ -> state, Myqueue.add queue e 
              in
              let state,queue = List.fold_left f (state,queue) complete in

              log @@ " here !! jobs " 
                ^ (string_of_int (List.length state.jobs)) 
                ^ " queue " 
                ^ (string_of_int (Myqueue.length queue )) 

              >>   
              (* why are we passing state around everywhere 
                when a seq job finishes it has to be able to return the new state...

                - we need to hand off the state to the sequence job...
                - not pass it around here...
              *)  

              (* take one event off the queue, only if one isn't already running ? *)
              let state,queue = 
                if queue <> Myqueue.empty then 
                  let e,queue = Myqueue.take queue in
  
                  (* we inject the state into the job which owns it for this action... 
                        there's a problem...
                        the jobs type returns event types so we can't define it... 
                        -----
                        why bother adding the thing jobs? because that's what we check to 
                        see what has advanced...
                        ----
                        we don't have to pass state in loop. instead just push a queue job
                        to return it...
                        --- 
                        VERY IMPORTANT
                        - can we structure the code so all the complex state manipulation
                        only happens in response to a seqJObFinished. which might mean
                        we won't have to swing around the state. except in the que
                        -----
                        ugghhh. there's still a whole ordering issue here.
                        -----
                        OK. rather than forcing everything through the single nchoose picker
                        ---- 

                        - have two io subsystems. jobs and sequence jobs
                            - choose completed state.jobs and push stuff onto queue... 
                            - process the queue and update state.
                        problem is using same queue in both. we cant thread/share 
                  *) 

                  let state = 
                    { state with
                      seq_job_running = true;
                      jobs = (
                        (* state is injected into the seq job *)
                        let state = P2p.update state e
                        in return Misc.SeqJobFinished
                      )
                        :: state.jobs;
                    }
                  in

                  state,queue 
                else
                  state,queue 
              in

            
              if List.length state.jobs > 0 then
                loop state queue
              else
                log "finishing - no more jobs to run!!"
                >> return ()
          )
            (fun exn ->
              (* must close *)
              let s = Printexc.to_string exn  ^ "\n" ^ (Printexc.get_backtrace () ) in
              log ("finishing - exception " ^ s )
              >> (* just exist cleanly *)
                return ()
            )
          in
          let queue = Myqueue.empty in 
          loop state queue
        )
  )

(*
let update state e =
  let state = P2p.update state e in
  let state = Chain.update state e in
  let state = Seq.update state e in
  state
*)

let () = run () 



