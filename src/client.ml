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

  queue : U.jobs_type Myqueue.t;

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


        let rec loop state queue jobs  =

          Lwt.catch (

            (* select completed jobs *)            
            fun () -> Lwt.nchoose_split jobs
            >>= fun (complete, incomplete) ->
              (* process io events *) 
              let f (state,queue) e = 
                match e with 
                  (* nop *)
                  | U.Nop -> state,queue
                  (* a seq job finished then take the new state *) 
                  | SeqJobFinished -> state,queue
                  (* any other event gets added to queue *)
                  | _ -> state, Myqueue.add queue e 
              in
              let state,queue = List.fold_left f (state,queue) complete in

              log @@ " here !! jobs " 
                ^ (string_of_int (List.length jobs)) 
                ^ " queue " 
                ^ (string_of_int (Myqueue.length queue )) 

              >>   
              (* take one event off the queue, wrap it as a job *)
              let jobs = incomplete in

              let queue,jobs = 
                if queue <> Myqueue.empty then 
                  let e,queue = Myqueue.take queue in
                  let jobs = (
                    (* state is injected into the seq job *)
                    let state = P2p.update state e
                    in return Misc.SeqJobFinished
                  ) :: jobs;
                  in
                  queue,jobs 
                else
                  queue,jobs 
              in
            
              if List.length jobs > 0 then
                loop state queue jobs
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
          loop state queue jobs
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



