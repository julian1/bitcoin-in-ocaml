(*
  corebuild -I src  -package pgocaml,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax src/client.byte
*)

let (>>=) = Lwt.(>>=)
let return = Lwt.return

module M = Message
module U = Misc


type whoot_t =
{
  state : U.my_app_state ;

  jobs : U.jobs_type; (* it's not a job it's a job completion code, or result or event *)

  queue : U.my_event Myqueue.t;

}


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


        let rec loop whoot =

          Lwt.catch (

            (* select completed jobs *)            
            fun () -> Lwt.nchoose_split whoot.jobs
            >>= fun (complete, incomplete) ->

              let whoot = { whoot with jobs = incomplete  } in 

              (* process io events *) 
              let f whoot e = 
                match e with 
                  (* nop *)
                  | U.Nop -> whoot 
                  (* a seq job finished then take the new state *) 
                  | SeqJobFinished -> whoot 
                  (* any other event gets added to queue *)
                  | _ -> { whoot with queue = Myqueue.add whoot.queue e }  
              in
              let whoot = List.fold_left f whoot complete in

              log @@ " here !! jobs " 
                ^ (string_of_int (List.length whoot.jobs)) 
                ^ " queue " 
                ^ (string_of_int (Myqueue.length whoot.queue )) 

              >>   

              (* take one event off the queue, wrap it as a job *)
              let whoot = 
                if whoot.queue <> Myqueue.empty then 
                  let e,queue = Myqueue.take whoot.queue in
                  {  whoot with  
                    jobs = (
                    (* state is injected into the seq job *)
                    let state = P2p.update whoot.state e
                    in return Misc.SeqJobFinished
                  ) :: whoot.jobs;
                  }
                else
                  whoot
              in
            
              if List.length whoot.jobs > 0 then
                loop whoot 
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
          in let queue = Myqueue.empty in 
          let jobs = P2p.create() in

          let whoot = {
            state = state; 
            jobs = jobs; 
            queue = queue;
          } in
          loop whoot 
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



