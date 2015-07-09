(*
  corebuild -I src  -package pgocaml,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax src/client.byte
*)

let (>>=) = Lwt.(>>=)
let return = Lwt.return

module M = Message
module U = Misc


type whoot_t =
{
  state : U.my_app_state option;

  jobs : U.jobs_type; (* it's not a job it's a job completion code, or result or event *)

  queue : U.my_event Myqueue.t;

}


let log s = U.write_stdout s


(*
let myupdate state e = 
log "running myupdate"
>> return (Misc.SeqJobFinished state)
*)


(*
  let state = P2p.update state e
  in return Misc.SeqJobFinished
*) 


let run () =

  Lwt_main.run (

    (* we'll have to think about db transactions *) 
    log "connecting and create db"
    >> U.PG.connect ~host:"127.0.0.1" ~database: "test" ~user:"meteo" ~password:"meteo" ()
    >>= fun db ->

      Processblock.create_prepared_stmts db 
    >>
(*
      Chain.create () 
    >>= fun blocks_fd -> 
    *)
       (
(*
    ok, what do we have to do now???

    - make seqjobfinished return new state and any additional jobs
    - this is a bit hard...

    - get rid of jobs in state.

    - done - boolean as to whether a seq job is running...  or use option state ?
*)
        let rec loop whoot =

          Lwt.catch (

            (* select completed jobs *)            
            fun () -> Lwt.nchoose_split whoot.jobs
            >>= fun (complete, incomplete) ->
        
              (* update incoplete jobs *)
              let whoot = { whoot with jobs = incomplete } in 

              (* process complete io events, by pushing on queue *) 
              let f whoot e = 
                match e with 
                  (* nop *)
                  | U.Nop -> whoot 
                  (* a seq job finished then take the new state *) 
                  | SeqJobFinished state -> { whoot with state = Some state } 
                  (* any other event gets added to queue *)
                  | _ -> { whoot with queue = Myqueue.add whoot.queue e }  
              in
              let whoot = List.fold_left f whoot complete in

              log @@ " here !! jobs " 
                ^ (string_of_int (List.length whoot.jobs)) 
                ^ " queue " 
                ^ (string_of_int (Myqueue.length whoot.queue )) 

              >>   

              (* take one event off the queue, add to incomplete jobs *)
              let whoot = 
                if whoot.queue <> Myqueue.empty then 
                  let e,queue = Myqueue.take whoot.queue in
                  {  whoot with  
                    queue = queue;
                    jobs = (
                      (* state is injected into the seq job, and nulled *)
                      let Some state = whoot.state in
                      P2p.update state e
                      (* rather than trying to process this here why not change the return function *) 
                      
                      (* let Some state = whoot.state in
                      myupdate state e  *)
                    ) :: whoot.jobs;
                    state = None;
                  }
                else
                  whoot
              in
            
              (* anything more to process *) 
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

        let whoot = {
          state = Some ({
            connections = [];
            db = db; 
            (* should be hidden ?? *)
            block_inv_pending  = None;
            blocks_on_request = U.SS.empty;
            last_block_received_time = [];
          } : U.my_app_state )
          ; 
          jobs = P2p.create(); 
          queue = Myqueue.empty ;
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



